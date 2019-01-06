library(ggplot2)
library(gridExtra)
library(dplyr)
library(magrittr)

# X=连续值，Y=连续值 => 对X箱型图、密度图, 对X、Y散点图+回归平滑
# X=离散值，Y=连续值 => 对X和Y箱型图、密度图
# X=连续值，Y=离散值 => 对X和Y箱型图、密度图
# X=离散值，Y=离散值 => 对X和Y直方图、气泡图（个数与占比）

# EDA
eda <- function(data, x, target){

  # X=连续值，Y=连续值 => 对X箱型图、密度图,对X、Y散点图+回归平滑
  if (is.numeric(data[[x]]) & is.numeric(data[[target]])){
    boxplot = ggplot(data, aes(x='x', y=data[[x]])) + geom_boxplot() +
      xlab(x) + ylab(x)
    density_plot = ggplot(data, aes(x=data[[x]])) +
      geom_density(position="identity") +
      xlab(x)
    scatter_plot = ggplot(data, aes_(x=data[[x]], y=data[[target]])) + geom_point() +
      geom_smooth() +
      xlab(x) + ylab(target)
    lay <- rbind(c(1,1,2,2,2,2),
                 c(1,1,2,2,2,2),
                 c(1,1,3,3,3,3),
                 c(1,1,3,3,3,3))
    grid.arrange(grobs = list(boxplot, density_plot, scatter_plot), layout_matrix = lay)
    # print(boxplot)
  }

  # X=离散值，Y=连续值 => 对X和Y箱型图、密度图
  if (is.factor(data[[x]]) & is.numeric(data[[target]])){
    density_plot = ggplot(data, aes(x=data[[target]], fill=data[[x]], alpha=0.1)) +
      geom_density(position="identity") +
      xlab(target) + scale_fill_discrete(name=x)

    boxplot = ggplot(data, aes(x=data[[x]], y=data[[target]])) + geom_boxplot() +
      xlab(x) + ylab(target)

    grid.arrange(density_plot, boxplot, ncol=2)
    # print(boxplot)
  }

  # X=连续值，Y=离散值 => 对X和Y箱型图、密度图
  if (is.numeric(data[[x]]) & is.factor(data[[target]])){
    density_plot = ggplot(data, aes(x=data[[x]], fill=data[[target]], alpha=0.1)) +
      geom_density(position="identity") +
      xlab(x) + theme(legend.position = "none")

    boxplot = ggplot(data, aes(x='x', y=data[[x]], fill=data[[target]])) + geom_boxplot() +
      xlab('') + ylab(x) + scale_fill_discrete(name=target)

    grid.arrange(density_plot, boxplot, ncol=2)
    # print(boxplot)
  }

  # X=离散值，Y=离散值 => 对X和Y直方图、气泡图（个数与占比）


}
# eda(data = data, x='Sepal.Width', target = 'Sepal.Length')

# 观察两个集合中的变量的分布差异
check_tt = function(train, test, x){

  # 从小到大排序，看差异
  train_x = train[[x]] %>% sort()
  test_x = test[[x]] %>% sort()
  df1 = data.frame(index = c(1:length(train_x)), train = train_x)
  df2 = data.frame(index = c(1:length(test_x)), test = test_x)
  df = df1 %>% left_join(df2, by='index') %>% melt(id='index')

  plot_1 = ggplot(data=df, aes(x=index, y=value, color=variable)) + geom_line() + ylab(x)

  # 看变量取值分布差异
  train_x = data.frame(x = train[[x]]) %>%
    tbl_df() %>%
    group_by(x) %>%
    summarise(num_train = n()) %>%
    mutate(train = round(num_train/sum(num_train),4))
  test_x = data.frame(x = test[[x]]) %>%
    tbl_df() %>%
    group_by(x) %>%
    summarise(num_test = n()) %>%
    mutate(test = round(num_test/sum(num_test),4))
  df = train_x %>% left_join(test_x, by='x') %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    select(-c(num_train, num_test)) %>%
    melt(id='x')
  plot_2 = ggplot(data = df, aes(x=x, y=value, color=variable)) +
    geom_line() + xlab(x) + ylab('ratio')

  grid.arrange(plot_1, plot_2, nrow=2)
}









