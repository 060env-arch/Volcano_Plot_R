# 1. 请确保已运行过 library(ggplot2), library(dplyr)
if (!requireNamespace("ggsci", quietly = TRUE)) install.packages("ggsci") # 提供更高级配色
library(ggsci)

## 2. 读取数据 (请确保 dummy_data.csv 与本脚本在同一文件夹)
# 实际使用时，把 dummy_data.csv 换成你自己的差异分析结果文件
data <- read.csv("D:/桌面/dummy_data.csv", header = TRUE)

# 3. 设置阈值参数
log2FC_threshold <- 1.0  # 差异倍数阈值 (通常设为 1 或 1.5)
pvalue_threshold <- 0.05 # 显著性阈值

# 4. 数据预处理
data <- data %>%
  mutate(
    Significance = case_when(
      log2FoldChange > log2FC_threshold & pvalue < pvalue_threshold ~ "Up",
      log2FoldChange < -log2FC_threshold & pvalue < pvalue_threshold ~ "Down",
      TRUE ~ "Not Sig"
    )
  )

# 5. 开始绘图
p <- ggplot(data, aes(x = log2FoldChange, y = -log10(pvalue), color = Significance)) +
  # 调整散点
  geom_point(shape = 21, size = 3.5, stroke = 1.2, alpha = 0.95) + 
  scale_color_manual(values = c("Up" = "#ff8c7a", "Down" = "#88dcb0", "Not Sig" = "#d0d0d0")) +
  # 填充颜色 
  scale_fill_manual(values = c("Up" = "#ffb3a7", "Down" = "#b1e7c9", "Not Sig" = "#e0e0e0")) +
  # 让颜色同时应用到边框和填充
  aes(fill = Significance) +
  
  # 优化辅助线：使用更细的实线或虚线，降低颜色对比
  geom_vline(xintercept = c(-log2FC_threshold, log2FC_threshold), linetype = "solid", color = "#eaeaea", size = 0.5) +
  geom_hline(yintercept = -log10(pvalue_threshold), linetype = "solid", color = "#eaeaea", size = 0.5) +
  
  # 优化全局主题
  theme_minimal() + # 使用极简主题作为基础
  labs(
    title = "Volcano Plot of Differential Abundance",
    x = "Log2 Fold Change",
    y = "-Log10(P-value)"
  ) +
  theme(
    # 调整标题：增大字体，居中，使用深灰色
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#333333"),
    # 调整坐标轴文字：增大字体，加粗
    axis.title = element_text(face = "bold", size = 13, color = "#555555"),
    axis.text = element_text(size = 11),
    # 移除网格线，制造类似 theme_classic 的干净感
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # 优化图例：放置在底部，水平排列
    legend.position = "bottom",
    legend.title = element_blank(), # 隐藏图例标题
    legend.text = element_text(size = 11)
  )

# 6. 保存为高分辨率 PDF，方便后期在 AI (Illustrator) 中修改排版
ggsave("D:/桌面/Volcano_Plot.pdf", plot = p, width = 6, height = 5, dpi = 300)
print("绘图完成！请在当前文件夹查看 Volcano_Plot.pdf")
