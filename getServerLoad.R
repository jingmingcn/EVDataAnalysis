getServerLoad <- function() {
  # Get CPU load average (1 minute) for macOS
  if (Sys.info()["sysname"] == "Darwin") {
    cpu_info <- system("sysctl -n vm.loadavg", intern = TRUE)
    # Parse the load average (format is "{ X.XX Y.YY Z.ZZ }")
    cpu_parts <- strsplit(gsub("[{}]", "", cpu_info), " ")[[1]]
    cpu_parts <- cpu_parts[cpu_parts != ""]  # Remove empty strings
    cpu_load <- as.numeric(cpu_parts[1])
    
    if (is.na(cpu_load)) {
      warning("Could not parse CPU load, using fallback method")
      cpu_load <- as.numeric(system("top -l 1 | grep 'CPU usage' | awk '{print $3}' | sed 's/%//'", intern = TRUE)) / 100
    }
    
    cpu_load <- cpu_load / parallel::detectCores()
  } else {
    # For Linux systems, use original method
    cpu_load <- system("cat /proc/loadavg", intern = TRUE)
    cpu_load <- as.numeric(strsplit(cpu_load, " ")[[1]][1])
    cpu_load <- cpu_load / parallel::detectCores()
  }
  
  # Get memory usage
  if (Sys.info()["sysname"] == "Darwin") {  # macOS
    mem_info <- system("vm_stat", intern = TRUE)
    # Parse memory info
    page_size <- 4096  # Default page size on macOS
    free_pages <- as.numeric(gsub(".*: ", "", mem_info[2]))
    active_pages <- as.numeric(gsub(".*: ", "", mem_info[3]))
    inactive_pages <- as.numeric(gsub(".*: ", "", mem_info[4]))
    wired_pages <- as.numeric(gsub(".*: ", "", mem_info[7]))
    
    total_mem <- as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
    used_mem <- (active_pages + wired_pages) * page_size
    mem_load <- used_mem / total_mem
  } else {  # Linux and others
    mem_info <- system("free", intern = TRUE)
    mem_values <- as.numeric(strsplit(mem_info[2], "\\s+")[[1]][-1])
    mem_load <- mem_values[2] / mem_values[1]
  }
  message(sprintf("CPU Load: %.2f, Memory Load: %.2f", cpu_load, mem_load))
  # Calculate combined load (maximum of CPU and memory load)
  combined_load <- max(cpu_load, mem_load)
  
  # Ensure the result is between 0 and 1
  return(min(max(combined_load, 0), 1))
}