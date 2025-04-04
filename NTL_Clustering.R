# ===============================
# Full Script: Nighttime Lights Clustering & Analysis
# ===============================

# Load necessary libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(RColorBrewer)
library(sf)        # Spatial data handling
library(ggspatial) # For North Arrow

# Load dataset
df <- read.csv("/STDM - NTL/SpatioTemporal_NTL_Central_South_JKT_FIX.csv", #Adjust the file path to your local dataset in this line
               sep=";", header=TRUE)

# Convert necessary columns to numeric
df <- df %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude),
    Brightness = as.numeric(Brightness),
    Year = as.numeric(Year)
  ) %>%
  filter(!is.na(Brightness) & is.finite(Brightness))

# ===========================================
# 1. BENCHMARK CLUSTERING (2012 Benchmark)
# ===========================================

df_2012 <- df %>% filter(Year == 2012)
df_scaled_2012 <- scale(df_2012[, c("Longitude", "Latitude", "Brightness")])
write.csv(df_scaled_2012, "/STDM - NTL/Normalized_2012_Data.csv", row.names=FALSE)

# --- K-Means Clustering ---
set.seed(42)
kmeans_result_2012 <- kmeans(df_scaled_2012, centers=5, nstart=25)
df_2012$KMeans_Cluster <- kmeans_result_2012$cluster

# --- Hierarchical Clustering ---
hc_model_2012 <- hclust(dist(df_scaled_2012), method="ward.D2")
df_2012$Hierarchical_Cluster <- cutree(hc_model_2012, k=5)

# ===========================================
# 2. Cluster Brightness Ranges from 2012
# ===========================================

kmeans_ranges <- df_2012 %>%
  group_by(KMeans_Cluster) %>%
  summarise(Min_Brightness = min(Brightness),
            Max_Brightness = max(Brightness),
            Mean_Brightness = mean(Brightness)) %>%
  arrange(Mean_Brightness) %>%
  mutate(Cluster = 1:5)

hierarchical_ranges <- df_2012 %>%
  group_by(Hierarchical_Cluster) %>%
  summarise(Min_Brightness = min(Brightness),
            Max_Brightness = max(Brightness),
            Mean_Brightness = mean(Brightness)) %>%
  arrange(Mean_Brightness) %>%
  mutate(Cluster = 1:5)

# Cluster assignment function
assign_clusters <- function(brightness, breaks) {
  sapply(brightness, function(x) which.min(abs(breaks - x)))
}

# ===========================================
# 3. CLUSTERING FOR EACH YEAR (Using 2012 Benchmark)
# ===========================================

unique_years <- unique(df$Year)
brightness_changes <- data.frame()
brightness_ranges <- data.frame()
df_all_clustered <- data.frame()

for (yr in unique_years) {
  df_year <- df %>% filter(Year == yr)
  if (nrow(df_year) == 0) next
  
  # Assign clusters
  df_year$KMeans_Cluster <- assign_clusters(df_year$Brightness, kmeans_ranges$Max_Brightness)
  df_year$Hierarchical_Cluster <- assign_clusters(df_year$Brightness, hierarchical_ranges$Max_Brightness)
  
  # Save all clustered data
  df_all_clustered <- bind_rows(df_all_clustered, df_year)
  
  # Save brightness summaries
  save_summary <- function(data, cluster_col, method_name) {
    data %>%
      group_by(!!sym(cluster_col)) %>%
      summarise(
        Mean_Brightness = mean(Brightness),
        Min_Brightness = min(Brightness),
        Max_Brightness = max(Brightness)
      ) %>%
      mutate(Method = method_name, Year = yr) %>%
      rename(Cluster = !!sym(cluster_col))
  }
  
  kmeans_summary <- save_summary(df_year, "KMeans_Cluster", "KMeans")
  hierarchical_summary <- save_summary(df_year, "Hierarchical_Cluster", "Hierarchical")
  
  brightness_changes <- bind_rows(brightness_changes, kmeans_summary, hierarchical_summary)
  brightness_ranges <- bind_rows(brightness_ranges, kmeans_summary, hierarchical_summary)
  
  # ---------------------------
  # Generate Cluster Maps
  # ---------------------------
  df_sf <- st_as_sf(df_year, coords = c("Longitude", "Latitude"), crs = 4326)
  cluster_colors <- c("lightyellow", "gold", "orange", "red", "darkred")
  
  plot_cluster <- function(data_sf, cluster_col, title_text, filename) {
    ggplot() +
      geom_sf(data = data_sf, aes_string(color = cluster_col), size = 0.5, alpha = 0.8) +
      scale_color_manual(values = cluster_colors, name = "Cluster") +
      labs(title = title_text, x = "Longitude", y = "Latitude") +
      annotation_north_arrow(location = "tr", which_north = "true", 
                             style = north_arrow_fancy_orienteering(text_size = 8, line_width = 0.3)) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(size = 8)) -> p
    ggsave(filename = filename, plot = p, width = 7, height = 5, dpi = 300)
  }
  
  plot_cluster(df_sf, "factor(KMeans_Cluster)", paste("K-Means Clustering for Year", yr),
               paste0("/STDM - NTL/Benchmark_KMeans_", yr, ".png")) #Adjust the file path to your local dataset in this line
  
  plot_cluster(df_sf, "factor(Hierarchical_Cluster)", paste("Hierarchical Clustering for Year", yr),
               paste0("/STDM - NTL/Benchmark_Hierarchical_", yr, ".png")) #Adjust the file path to your local dataset in this line
}

# Save brightness data
write.csv(brightness_ranges, "/STDM - NTL/Cluster_Brightness_Ranges.csv", row.names = FALSE) #Adjust the file path to your local dataset in this line

cat("\n✅ K-Means & Hierarchical Clustering (k=5) using 2012 Benchmark with brightness range tracking complete.\n")

# ===========================================
# 4. SILHOUETTE ANALYSIS (2012 Benchmark)
# ===========================================

# K-Means Silhouette
silhouette_kmeans <- silhouette(kmeans_result_2012$cluster, dist(df_scaled_2012))
avg_silhouette_kmeans <- mean(silhouette_kmeans[, 3])
cat("\n✅ Average Silhouette Score for K-Means Clustering (2012):", round(avg_silhouette_kmeans, 4), "\n")
write.csv(as.data.frame(silhouette_kmeans), 
          "/STDM - NTL/KMeans_Silhouette_2012.csv", #Adjust the file path to your local dataset in this line
          row.names = FALSE)

# Hierarchical Silhouette
silhouette_hierarchical <- silhouette(df_2012$Hierarchical_Cluster, dist(df_scaled_2012))
avg_silhouette_hierarchical <- mean(silhouette_hierarchical[, 3])
cat("\n✅ Average Silhouette Score for Hierarchical Clustering (2012):", round(avg_silhouette_hierarchical, 4), "\n")
write.csv(as.data.frame(silhouette_hierarchical), 
          "/STDM - NTL/Hierarchical_Silhouette_2012.csv", #Adjust the file path to your local dataset in this line
          row.names = FALSE)

# ===========================================
# 5. COUNT POINTS PER CLUSTER PER YEAR
# ===========================================

# Prepare count summary for K-Means
kmeans_count <- df_all_clustered %>%
  group_by(Year, KMeans_Cluster) %>%
  summarise(Count = n()) %>%
  mutate(Method = "KMeans") %>%
  rename(Cluster = KMeans_Cluster)

# Prepare count summary for Hierarchical Clustering
hierarchical_count <- df_all_clustered %>%
  group_by(Year, Hierarchical_Cluster) %>%
  summarise(Count = n()) %>%
  mutate(Method = "Hierarchical") %>%
  rename(Cluster = Hierarchical_Cluster)

# Combine and export
cluster_distribution <- bind_rows(kmeans_count, hierarchical_count)
write.csv(cluster_distribution,
          "/STDM - NTL/Cluster_Distribution_Per_Year.csv", #Adjust the file path to your local dataset in this line
          row.names = FALSE)

cat("\n✅ Cluster distribution summary saved.\n")
