library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(stringr)
library(Seurat)
library(readr)
library(purrr)
library(harmony)
library(scales)

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "minty"),
  title = "EV Data Analysis and Visualization",
  sidebar = sidebar(
    fileInput("evFiles", "Choose EV CSV Files", multiple = TRUE, accept = ".csv"),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$evFiles)
  })
  
  output$scatter <- renderPlot(
    {
      seurat_list <- list()
      if(is.null(input$evFiles))
        return()
      for (i in 1:nrow(input$evFiles)) {
        row <- input$evFiles[i,]
        message("Processing: ", row$name)
        
        # === 读取长格式数据 ===
        sc_data <- fread(row$datapath, data.table = FALSE)
        colnames(sc_data) <- c("symbol", "ev", "variable", "value")
        sc_data$value <- as.numeric(sc_data$value)
        
        sample_name <- unique(sc_data$symbol)
        if (length(sample_name) != 1) {
          warning("Multiple or missing sample names in file: ", basename(file))
          next
        }
        
        # === 长转宽格式 ===
        dat_wide <- pivot_wider(sc_data,
                                id_cols = ev,
                                names_from = variable,
                                values_from = value,
                                values_fill = 0)
        rownames(dat_wide) <- dat_wide$ev
        dat_wide <- dat_wide[, -1, drop = FALSE]
        
        # === 筛选 EV（至少1个蛋白表达值 ≥ 1）===
        valid_ev_idx <- apply(dat_wide, 1, function(x) sum(x >= 1) >= 2)
        dat_filtered <- dat_wide[valid_ev_idx, , drop = FALSE]
        n_ev <- nrow(dat_filtered)
        
        message("Number of EVs in ", sample_name, ": ", n_ev)
        
        group_size <- 10
        
        num_groups <- floor(n_ev / group_size)
        if (num_groups == 0) {
          message("EVs < group size in ", sample_name, ", skip.")
          next
        }
        
        # === 随机分组合并生成伪EV ===
        set.seed(123)
        sampled_ev_idx <- sample(1:n_ev, num_groups * group_size, replace = FALSE)
        dat_sub <- dat_filtered[sampled_ev_idx, , drop = FALSE]
        groups <- rep(1:num_groups, each = group_size)
        
        pseudo_ev_list <- lapply(unique(groups), function(g) {
          rows <- which(groups == g)
          colSums(dat_sub[rows, , drop = FALSE])
        })
        names(pseudo_ev_list) <- paste0(sample_name, "_sEV", seq_along(pseudo_ev_list))
        
        # ===  合并为矩阵（蛋白为行，伪EV为列） ===
        pseudo_matrix <- do.call(cbind, pseudo_ev_list)
        pseudo_matrix <- as.data.frame(pseudo_matrix)
        pseudo_matrix <- pseudo_matrix[order(rownames(pseudo_matrix)), ]
        
        # ===  SeuratObject ===
        seu <- CreateSeuratObject(counts = pseudo_matrix,
                                  project = sample_name,
                                  min.features = 2,
                                  min.cells = 1)
        seurat_list[[sample_name]] <- seu
        
      }
      
      if (length(seurat_list) >= 2) {
        message("Merging ", length(seurat_list), " Seurat objects...")
        merged_seu <- merge(seurat_list[[1]], y = seurat_list[-1], project = "MergedPseudoEVs")
        merged_seu$orig.ident <- sapply(strsplit(colnames(merged_seu), "_"), `[`, 1)
        merged_seu <- SCTransform(merged_seu, return.only.var.genes = FALSE, verbose = TRUE)
        merged_seu <- RunPCA(merged_seu, npcs = 30, assay = "SCT", verbose = FALSE)
        merged_seu <- RunHarmony(merged_seu, group.by.vars = "orig.ident", plot_convergence = TRUE)
        
        dims_use <- 1:ncol(Embeddings(merged_seu, "harmony"))
        merged_seu <- merged_seu %>%
          RunUMAP(reduction = "harmony", dims = dims_use, n.neighbors = 20, min.dist = 0.2, spread = 1) %>%
          RunTSNE(reduction = "harmony", dims = dims_use, perplexity = 50, learning.rate = 1000, theta = 0.5, check_duplicates = FALSE) %>%
          FindNeighbors(reduction = "harmony", dims = dims_use) %>%
          FindClusters(resolution = 0.4)
        
        Idents(merged_seu) <- "seurat_clusters"
        merged_seu$seurat_clusters  <- factor(merged_seu$seurat_clusters )
        cluster_table <- table(merged_seu$seurat_clusters )
        cluster_prop <- prop.table(cluster_table) * 100
        cluster_labels <- paste0(names(cluster_prop), " (", round(cluster_prop, 1), "%)")
        names(cluster_labels) <- names(cluster_prop)
        merged_seu$cluster_with_pct <- plyr::mapvalues(
          merged_seu$seurat_clusters ,
          from = names(cluster_labels),
          to = cluster_labels
        )
        
        tsne_coords <- Embeddings(merged_seu, "tsne")
        tsne_df <- data.frame(
          cell = rownames(tsne_coords),
          tSNE_1 = tsne_coords[, 1],
          tSNE_2 = tsne_coords[, 2],
          cluster = merged_seu$cluster_with_pct
        )
        #write_json(tsne_df, path = file.path(output_matrix_path, "tsne_results.json"), pretty = TRUE)
        
        p <- DimPlot(
          merged_seu,
          reduction = "tsne",
          group.by = "cluster_with_pct",
          pt.size = 0.5,
          raster = TRUE
        ) +
          ggtitle("t-SNE") +
          theme_minimal() +
          theme(
            panel.background = element_rect(fill = "gray95", color = NA),
            plot.title = element_text(hjust = 0.5, size = 18),
            legend.text = element_text(size = 12)
          )
        
        
        p
      }
    },
    res = 100
  )
}

shinyApp(ui, server)
