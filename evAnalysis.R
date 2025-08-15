evAnalysis <- function(data_files, output_dir, sampling_ratio = 4, min_proteins = 2, min_count = 2) {

  # Create and return a promise
  promises::promise(function(resolve, reject) {

    # Create progress handler
    withProgress(message = 'Analysis in progress', detail = 'Starting...! This might take a moment...!', value = 0, {
    
      tryCatch({
        # Initialize results list
        return_results <- list()

        # 第一步：数据读取和预处理
        all_long_data <- data.frame()
        n <- length(data_files)
        incProgress(1/10, detail = "Loading and preprocessing data...")
        for (data_file in data_files) {
          file_path <- data_file$datapath
          sample_name <- tools::file_path_sans_ext(basename(file_path))
          message("Processing: ", sample_name)
          incProgress(1/10/n, detail = paste("Processing: ", sample_name))
          
          dt <- fread(file_path)
          message("Data columns: ", paste(colnames(dt), collapse = ", "))
          message("Data dimensions: ", nrow(dt), " x ", ncol(dt))
          
          dt_filtered <- dt %>%
            group_by(ev) %>%
            dplyr::filter(sum(value >= min_count) >= min_proteins) %>%
            ungroup()
          message("After filtering: ", nrow(dt_filtered), " rows")
          
          if (sampling_ratio > 1) {
            set.seed(123)
            dt_filtered_dt <- as.data.table(dt_filtered)
            sampled_evs <- dt_filtered_dt[, {
              unique_evs <- unique(ev)
              sample_size <- max(1, floor(length(unique_evs) / sampling_ratio))
              sampled <- sample(unique_evs, size = sample_size)
              list(ev_sampled = sampled)
            }, by = symbol]
            sampled_evs_expanded <- sampled_evs[, .(ev = unlist(ev_sampled)), by = symbol]
            
            dt_sampled <- dt_filtered_dt[sampled_evs_expanded, on = c("symbol", "ev"), nomatch = 0]
            dt_sampled <- as.data.frame(dt_sampled)
          } else {
            dt_sampled <- dt_filtered
          }    
          if (nrow(dt_sampled) == 0) {
            message("Warning: No data after sampling for ", sample_name)
            next
          }  
          all_long_data <- rbind(all_long_data, dt_sampled)
        }

        # 验证Group列是否存在
        if (!"group" %in% colnames(all_long_data)) {
          showNotification("Error: 'group' column not found in data. Please ensure your data contains a 'group' column.", type = "error", duration = NULL)
          stop("Error: Group column not found in data. Please ensure your data contains a 'group' column.")
        }        
        group_summary <- table(all_long_data$group)

        incProgress(1/10, detail = "Data loaded and preprocessing....")
        # 第二步：数据转换和标准化
        if (nrow(all_long_data) > 0) {
          
          # 转换为宽格式矩阵（EV × protein）
          temp_data <- all_long_data[, c("ev", "variable", "value")]
          mat_wide <- dcast(temp_data, ev ~ variable, value.var = "value", fill = 0, fun.aggregate = sum)
          
          ev_ids <- mat_wide$ev
          mat_counts <- as.matrix(mat_wide[, -1])
          rownames(mat_counts) <- ev_ids
          
          original_protein_matrix <- mat_counts  
          sample_info <- all_long_data %>%
            select(symbol, ev, group) %>%  
            distinct() %>%
            rename(sample = symbol, ev_id = ev, group = group)
          
          row_sums <- rowSums(mat_counts)
          valid_rows <- row_sums > 0
          if (sum(valid_rows) == 0) {
            showNotification("Error: All EVs have zero counts after filtering!", type = "error", duration = NULL)
            stop("Error: All EVs have zero counts!")
          }
          mat_counts_valid <- mat_counts[valid_rows, ]
          sample_info_valid <- sample_info[sample_info$ev_id %in% rownames(mat_counts_valid), ]
          
          # CPM标准化: (count / total_count) * 10^4
          cpm_data <- sweep(mat_counts_valid, 1, rowSums(mat_counts_valid), FUN = "/") * 1e4
          
          # Z-scale标准化
          col_sds <- apply(cpm_data, 2, sd, na.rm = TRUE)
          valid_cols <- col_sds > 0 & !is.na(col_sds)
          
          if (sum(valid_cols) == 0) {
            showNotification("Error: All proteins have zero variance after filtering!", type = "error", duration = NULL)
            stop("Error: All proteins have zero variance!")
          }
          
          cpm_valid <- cpm_data[, valid_cols]
          cpm_scaled <- scale(cpm_valid)
          
          if (any(is.na(cpm_scaled))) {
            message("Warning: NA values after scaling, replacing with 0")
            cpm_scaled[is.na(cpm_scaled)] <- 0
          }
          
          # 第三步：主成分分析(PCA)
          incProgress(1/10, detail = "Performing PCA...")
          if (nrow(cpm_scaled) < 2 || ncol(cpm_scaled) < 2) {
            showNotification("Error: Insufficient data for PCA!", type = "error", duration = NULL)
            stop("Error: Insufficient data for PCA")
          }
          
          pca_result <- prcomp(cpm_scaled, center = FALSE, scale. = FALSE)
          
          # 选择主成分数量（保留95%方差，最多18个PC）
          var_explained <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
          n_pc_95 <- which(var_explained >= 0.95)[1]
          
          if (is.na(n_pc_95)) {
            n_pc <- min(ncol(pca_result$x), 18)
          } else {
            n_pc <- min(n_pc_95, 18)
          }
          
          pca_data <- pca_result$x[, 1:n_pc, drop = FALSE]
          
          write.csv(pca_data, file = file.path(output_dir, "flowsom_input_data.csv"), row.names = TRUE)
          write.csv(sample_info_valid, file = file.path(output_dir, "sample_metadata.csv"), row.names = FALSE)
          write.csv(original_protein_matrix, file = file.path(output_dir, "original_protein_matrix.csv"), row.names = TRUE)
            
        } else {
          showNotification("Error: No valid data found after preprocessing!", type = "error", duration = NULL)
          stop("Error: No valid data found!")
        }

        # 第四步：FlowSOM聚类分析
        incProgress(1/10, detail = "Clustering with FlowSOM...")
        pca_data <- read.csv(file.path(output_dir, "flowsom_input_data.csv"), row.names = 1)
        data <- as.matrix(pca_data)

        meta <- NULL
        meta$name <- colnames(data)         
        meta$desc <- colnames(data)         
        meta$minRange <- apply(data, 2, min)  
        meta$maxRange <- apply(data, 2, max)  
        meta$range <- meta$maxRange - meta$minRange 
        meta <- data.frame(meta)

        ff <- new("flowFrame", 
                  exprs = as.matrix(data),
                  parameters = AnnotatedDataFrame(meta))

        fSOM <- ReadInput(ff, compensate = FALSE, transform = FALSE, scale = FALSE)
        fSOM <- BuildSOM(fSOM, xdim = 10, ydim = 10)

        # 第五步：Meta-clustering和最佳聚类数选择
        incProgress(1/10, detail = "Meta-clustering and determining optimal clusters...")
        code <- fSOM$map$codes
        rownames(code) <- 1:nrow(code)
        results <- ConsensusClusterPlus(
          t(code),
          maxK = 70,
          seed = 42,
          verbose = FALSE,
          writeTable = FALSE,
          plot = FALSE
        )
        PAC <- rep(0, 70)
        for(i in 2:70){
          mm <- results[[i]]$consensusMatrix
          fn <- ecdf(mm[lower.tri(mm)])
          PAC[i] <- integrate(fn, 0, 1, subdivisions = 2000)$value
        }

        PAC <- as.numeric(PAC)
        pac_diff <- diff(PAC[1:69])
        valid_indices <- which(!pac_diff < 0.01)

        if(length(valid_indices) > 0) {
          cluster_n <- max(valid_indices) + 2
        } else {
          cluster_n <- which.min(PAC[2:70]) + 1
        }

        cluster_n <- max(2, min(cluster_n, 70))
        message("Optimal cluster number selected: ", cluster_n, " (based on PAC analysis)")

        cluster_assignment <- results[[cluster_n]]$consensusClass
        fSOM$metaclustering <- cluster_assignment
        ev_clusters <- cluster_assignment[fSOM$map$mapping[,1]]
        cluster_percentages <- table(ev_clusters) / length(ev_clusters) * 100
        message("Clustering completed with ", cluster_n, " clusters")

        # 第六步：t-SNE可视化
        incProgress(1/10, detail = "Performing t-SNE for visualization...")
        perplexity <- as.integer(nrow(data)/450)
        perplexity <- max(45, min(perplexity, 440))

        # 执行t-SNE
        set.seed(42)
        tsne_out <- Rtsne(data, 
                          normalize = FALSE,
                          theta = 0.1,
                          max_iter = 1000,
                          eta = 800,
                          dims = 2,
                          num_threads = 32,
                          pca = FALSE,
                          perplexity = perplexity,
                          check_duplicates = FALSE,
                          final_momentum = 0)
        message("t-SNE completed with perplexity: ", perplexity)

        # 可视化
        clustering_results <- data.frame(
          tSNE_1 = tsne_out$Y[, 1],        
          tSNE_2 = tsne_out$Y[, 2],        
          Meta_Cluster = ev_clusters,      
          sample_id = rownames(data),
          stringsAsFactors = FALSE
        )
        message("t-SNE results prepared for visualization.")

        cluster_stats <- clustering_results %>%
          group_by(Meta_Cluster) %>%
          summarise(count = n(), .groups = 'drop') %>%
          mutate(percentage = round(count / sum(count) * 100, 1)) %>%
          mutate(cluster_label = paste0("Cluster ", Meta_Cluster, " (", percentage, "%)"))
        
        clustering_results_plot <- clustering_results %>%
          left_join(cluster_stats, by = "Meta_Cluster")
        message("Cluster statistics prepared for plotting.")
        tsne_plot <- ggplot(clustering_results_plot, aes(x = tSNE_1, y = tSNE_2, color = factor(cluster_label))) +
          geom_point(size = 0.8, alpha = 0.7) +
          labs(title = "t-SNE Visualization of EV Clusters", 
              subtitle = paste("Perplexity =", perplexity, "| Total Clusters =", max(clustering_results$Meta_Cluster, na.rm = TRUE)),
              x = "t-SNE 1", 
              y = "t-SNE 2", 
              color = "Cluster") +
          theme_void() +
          theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "right",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10),
            axis.title = element_text(size = 12)
          ) +
          guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
        
        return_results$plot <- tsne_plot

        ggsave(file.path(output_dir, "tsne_clusters_enhanced.pdf"), 
              tsne_plot, width = 16, height = 8, dpi = 300)

        message("t-SNE plot saved to output directory.")

        # 第七步：生成JSON报告
        incProgress(1/10, detail = "Generating JSON report...")
        original_matrix <- read.csv(file.path(output_dir, "original_protein_matrix.csv"),
                            row.names = 1, stringsAsFactors = FALSE)
        sample_info <- read.csv(file.path(output_dir, "sample_metadata.csv"), stringsAsFactors = FALSE)
        ev_to_symbol <- setNames(sample_info$sample, sample_info$ev_id)
        ev_to_group <- setNames(sample_info$group, sample_info$ev_id)
        ev_ids <- rownames(data)
        symbol_names <- ev_to_symbol[ev_ids]
        group_names <- ev_to_group[ev_ids]

        incProgress(1/10, detail = "Preparing data files for eVisualizer...")
        #  1. 生成 samplesheet.csv 
        samplesheet <- data.frame(
          Sample_Name = symbol_names,
          Group = group_names,
          stringsAsFactors = FALSE
        )
        write.csv(samplesheet, file = file.path(output_dir, "samplesheet.csv"),
                  row.names = FALSE, quote = FALSE)
        message("Sample groups distribution:")

        #  2. 生成 ev_protein_data.csv 
        matched_matrix <- original_matrix[ev_ids, ]
        ev_protein_data <- data.frame(
          index = seq_len(nrow(matched_matrix)),
          symbol = symbol_names,
          matched_matrix,
          stringsAsFactors = FALSE
        )
        write.csv(ev_protein_data, file = file.path(output_dir, "ev_protein_data.csv"),
                  row.names = FALSE, quote = FALSE)
        message("EV protein data saved.")

        #  3. 生成 tsne.csv 
        tsne_coords <- tsne_out$Y
        colnames(tsne_coords) <- c("X1", "X2")
        cluster_table <- table(ev_clusters)
        total_samples <- length(ev_clusters)
        cluster_ids <- sort(unique(ev_clusters))
        cluster_pct <- sapply(cluster_ids, function(x) {
          round(sum(ev_clusters == x) / total_samples * 100, 2)
        })
        for(i in seq_along(cluster_ids)) {
          message(sprintf("  Cluster %d: %d samples (%.2f%%)", 
                          cluster_ids[i], 
                          sum(ev_clusters == cluster_ids[i]), 
                          cluster_pct[i]))
        }
        cluster_label_map <- setNames(
          paste0("cluster", cluster_ids, "_(", cluster_pct, "%)"),
          as.character(cluster_ids)
        )
        tsne_data <- data.frame(
          index = seq_len(nrow(tsne_coords)),
          X1 = tsne_coords[, 1],
          X2 = tsne_coords[, 2], 
          cluster = cluster_label_map[as.character(ev_clusters)],
          symbol = symbol_names,
          stringsAsFactors = FALSE
        )

        write.csv(tsne_data, file = file.path(output_dir, "tsne.csv"),
                  row.names = FALSE, quote = FALSE)
        message("t-SNE coordinates saved.")

        #  4. 生成 panel.csv
        protein_names <- colnames(original_matrix)
        panel_data <- data.frame(
          index = 1:length(protein_names),
          symbol = protein_names,
          stringsAsFactors = FALSE
        )
        write.table(panel_data, 
                    file = file.path(output_dir, "panel.csv"),
                    sep = ",",
                    row.names = FALSE, 
                    col.names = FALSE,  
                    quote = FALSE)
        message("Panel data saved.")

        # 执行JSON生成
        incProgress(1/10, detail = "Generating JSON report...")
        json_data <- generate_evisualizer_json(
          output_dir = output_dir,
          report_name = "ARZ Human Protein Analysis",
          orderer_name = "researcher"
        )
        message("JSON report generated.")
        
        return_results$json <- list(
          content = json_data,
          path = file.path(output_dir, "analysis_report.json")
        )
        
        # Resolve the promise with results
        resolve(return_results)
        
      }, error = function(e) {
        # Reject the promise if there's an error
        reject(e)
      })
    }) # end of withProgress
  }) # end of promise
} # end of evAnalysis function