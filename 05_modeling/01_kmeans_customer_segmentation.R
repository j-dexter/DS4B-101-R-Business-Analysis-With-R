# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 CUSTOMER TRENDS ----
# - GOAL: Mine Customer Purchase History for similarity to other "like" customers
# - TECHNIQUES: K-Means Clustering, UMAP 2D Projection

# 1.1 Get Customer Trends ----

#* To get trends, we need to get the VERY granular/detailed data into 
    # it's aggregated form so that we can see trends.
    # k-means needs less-granular data in order to pick up trends.

#* protip: aggregating purchasing trends to cusotmers & products is 
    # typically the way to go.

#* protip: when understanding customer trends, it's important to collect:
    # 1) the unique customer name
    # 2) attributes related to the product
    # 3) a value to measure e.g. quantity or total price

#* protip: we convert to customer trends by:
    # 1) aggregating w/in customer-product groups, then
    # 2) normalizing w/in customer groups to get percentages of
            # product purchases by customer.

#* we are trying to get an 'essence' of what the customer likes

customer_trends_tbl <- bike_orderlines_tbl %>% 
    
    select(bikeshop_name, price, model, category_1, category_2, frame_material, quantity) %>% 
    
    # Group by and summarizations
    #* 1) aggregating w/in customer-product groups
    group_by(bikeshop_name, price, model, category_1, category_2, frame_material) %>% 
    summarize(quantity_purchased = sum(quantity)) %>% 
    ungroup() %>% 
    
    # Get Proportions to Standardize (for comparison purposes)
    # 2) normalizing data w/in customer groups to get percentages of
            # product purchases by customer (proportions help kmeans).
            # proportions allow kmeans to compare customers side-by-side.
    group_by(bikeshop_name) %>% 
    mutate(prop_of_total = quantity_purchased / sum(quantity_purchased)) %>% 
    ungroup()

customer_trends_tbl


# 1.2 Convert to User-Item Format (e.g. Customer-Product) ----

customer_product_tbl <- customer_trends_tbl %>% 
    
    select(bikeshop_name, model, prop_of_total) %>% 
    spread(key = model, value = prop_of_total, fill = 0)

customer_product_tbl


# 2.0 MODELING: K-MEANS CLUSTERING ----

# 2.1 Performing K-Means ----
?kmeans

#* nstart: k-means picks a random starting point and then iteratively finds
    # the best location for the centers. choosing nstart > 1 ensures
    # higher likelihood that a good center is found.
    # basically it gives a better solution.

kmeans_obj <- customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    kmeans(centers = 5, nstart = 100)

kmeans_obj$centers
kmeans_obj$cluster

# 2.2 Tidying a K-Means Object ----

broom::tidy(kmeans_obj) %>% glimpse()

broom::glance(kmeans_obj)

broom::augment(kmeans_obj, customer_product_tbl) %>% 
    select(bikeshop_name, .cluster) %>% 
    arrange(.cluster)

# 2.3 How many centers (customer groups) to use? ----

# Function that works on 1 element
centers <- 3

kmeans_mapper <- function(centers = 3) {
    
    customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        kmeans(centers = centers, nstart = 100)
}

3 %>% kmeans_mapper() %>% glance()

#* protip: we can apply broom:glance() row-wise with mutate() + map()

# Mapping the function to many elements
kmeans_mapped_tbl <- tibble(centers = 1:15) %>% 
    mutate(k_means = centers %>% map(kmeans_mapper)) %>% 
    mutate(glance  = k_means %>% map(glance))

kmeans_mapped_tbl %>% 
    unnest(glance) %>% 
    select(centers, tot.withinss)

# 2.4 Skree Plot ----

kmeans_mapped_tbl %>% 
    unnest(glance) %>% 
    select(centers, tot.withinss) %>% 
    
    # Visualization
    ggplot(aes(centers, tot.withinss)) +
    geom_point(color = "#2c3e50", size = 3) +
    geom_line(color = "#2c3e50", size = 1) +
    ggrepel::geom_label_repel(aes(label = centers), color = "#2c3e50") +
    
    # Formatting
    theme_tq() +
    labs(
        title = "Skree Plot",
        subtitle = "Measures the distance each of the customers are from the closest K-Means center",
        caption = "Based on the Scree Plot, we select 4 clusters to segment the customer base."
    )

# 3.0 VISUALIZATION: UMAP ----

#* UMAP: a dimensionality reduction technique that captures the structure
    # of a high-dimension data set (many numeric columns) in a two column
    # (x and y) data set.

# 3.1 Use UMAP to get 2-D Projection ----
?umap

umap_obj <- customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    umap()

umap_results_tbl <- umap_obj$layout %>% 
    as_tibble() %>% 
    set_names(c('x', 'y')) %>% 
    bind_cols(
        customer_product_tbl %>% select(bikeshop_name)
    )

umap_results_tbl %>% 
    ggplot(aes(x, y)) +
    geom_point() +
    geom_label_repel(aes(label = bikeshop_name), size = 3)

# 3.2 Use K-Means to Add Cluster Assignments ----
umap_results_tbl

kmeans_4_obj <- kmeans_mapped_tbl %>% 
    pull(k_means) %>% 
    pluck(4)

kmeans_4_clusters_tbl <- kmeans_4_obj %>% 
    augment(customer_product_tbl) %>% 
    select(bikeshop_name, .cluster)

umap_kmeans_4_results_tbl <- umap_results_tbl %>% 
    left_join(kmeans_4_clusters_tbl)

# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----

umap_kmeans_4_results_tbl %>% 
    mutate(label_text = str_glue("Customer: {bikeshop_name}
                                 Cluster: {.cluster}")) %>% 
    ggplot(aes(x, y, color = .cluster)) +
    
    # Geometries
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 2.5) +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(title    = "Customer Segmentation: 2D Projection",
         subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
         caption  = "Conclusion: 4 Customer Segments identified using 2 algorithms")

    

# 4.0 ANALYZE PURCHASING TRENDS ----

#* 2D projection now shows which customers are related...
#* however, we don't know what those customers w/in clusters are buying...
#* we need to figure that out... But how...?

# Next Step: Related Clusters to Products

#* study prices and determine bins
customer_trends_tbl %>% 
    pull(price) %>% 
    quantile(probs = c(0, 0.33, 0.66, 1))

?quantile

cluster_trends_tbl <- customer_trends_tbl %>% 
    
    # Join Cluster Assignment by Bikeshop Name
    left_join(umap_kmeans_4_results_tbl) %>% 
    
    mutate(price_bin = case_when(
        price <= 2240 ~ "low",
        price <= 4260 ~ "medium",
        TRUE ~ "high"
    )) %>%
    
    select(.cluster, model, contains("price"), category_1:quantity_purchased) %>% 
    
    # Aggregate quantity purchased by cluster and product attributes
    group_by_at(.vars = vars(.cluster:frame_material)) %>% 
    summarize(total_quantity = sum(quantity_purchased)) %>% 
    ungroup() %>% 
    
    # Normalize data by Calculating Proportion of Total
    group_by(.cluster) %>% 
    mutate(prop_of_total = total_quantity / sum(total_quantity)) %>% 
    ungroup()

cluster_trends_tbl


#* ASSESS TRENDS TO SHOW MARKETING THE PREFERENCES BY CLUSTER
    # just trying to get a sense of any trends that jump out

# Cluster 1 - Low/Medium Price, Road, Aluminum Frame
cluster_trends_tbl %>% 
    filter(.cluster == 1) %>% 
    arrange(desc(prop_of_total)) %>% 
    mutate(cum_prop = cumsum(prop_of_total)) %>% View()

get_cluster_trends <- function(cluster = 1) {
    
    cluster_trends_tbl %>% 
        filter(.cluster == cluster) %>% 
        arrange(desc(prop_of_total)) %>% 
        mutate(cum_prop = cumsum(prop_of_total)) 
}

# test function
get_cluster_trends(cluster = 1)

# Cluster 2 - High End Price, Road, Carbon Frame
get_cluster_trends(cluster = 2) 

# Cluster 3 - Low/Medium Price, Mountain, Aluminum Frame
get_cluster_trends(cluster = 3) 

# Cluster 4 - High End price, Mountain, Carbon Frame
get_cluster_trends(cluster = 4) 


# Update Visualization 

cluster_label_tbl <- tibble(
    .cluster = 1:4,
    .cluster_label = c(
        "Low/Medium Price, Road, Aluminum Frame",
        "High End Price, Road, Carbon Frame",
        "Low/Medium Price, Mountain, Aluminum Frame",
        "High End price, Mountain, Carbon Frame"
    )) %>% 
    mutate(.cluster = as_factor(.cluster))

cluster_label_tbl

# Join descriptions + visualize

umap_kmeans_4_results_tbl %>% 
    left_join(cluster_label_tbl) %>% 
    mutate(label_text = str_glue("Customer: {bikeshop_name}
                                 Cluster: {.cluster}
                                 {.cluster_label}")) %>% 
    ggplot(aes(x, y, color = .cluster)) +
    
    # Geometries
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 2.5) +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(title    = "Customer Segmentation: 2D Projection",
         subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
         caption  = "Conclusion: 4 Customer Segments identified using 2 algorithms")


