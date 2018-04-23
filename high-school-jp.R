# ライブラリの読み込み
library("tidyverse") # データ分析の定番
library("readxl") # Excel ファイルの読み込み
library("magrittr") # パイプ処理を分かりやすく
library("stringr") # 文字列処理

# Excel ファイル名が入ったベクトルの作成
xls_path <- "./original-data/"
fnames <- dir(pattern = "\\.xlsx?$", path = xls_path)

# 1つのExcelファイルを読みこんで、クリーニングする関数
extract_school_num <- function(fname){
  year <- as.integer(str_sub(fname, 1, 4))
  # Excelファイルの読み込み
  data <- read_excel(paste0(xls_path, fname),
                     col_names = FALSE)
  # データのクリーニング
  data %>% 
    # データに対する注釈的要素の除去、空行の除去
    filter(rowSums(is.na(.)) < ncol(.)-2) %>%
    # 空列の除去
    select_if(colSums(is.na(.)) != nrow(.)) %>%
    # 変数に見出しをつける
    inset(1, 1, "設置者") %>%
    inset(2, 1, "本校分校") %>% 
    # 転置による行と列の入れ替え
    t() %>%
    as_data_frame() %>%
    # 列名の設定
    set_colnames(.[1,]) %>%
    slice(-1) %>%
    # 文字列内の余計な空白の除去
    mutate(設置者 = str_replace_all(設置者, "\\s+", "")) %>%
    fill("設置者") %>%
    # 残った変数を列で表現する
    gather(key = 男女, value = 学校数,
           -one_of("設置者", "本校分校")) %>%
    # 合計を示すデータの削除
    filter_all(all_vars(!str_detect(., "計"))) %>%
    # 学校数ゼロを示すものの処理 
    mutate(学校数 = str_replace_all(学校数, "[―－]", "0")) %>%
    mutate(学校数 = as.integer(学校数)) %>%
    # 年を示すデータの追加
    mutate(年 = year) -> data
  return(data)
}

school_num <- map_df(fnames, extract_school_num)

# グラフの作成

school_num %>% 
  # 下準備
  group_by(男女, 年) %>%
  summarise(学校数 = sum(学校数)) %>%
  ungroup() %>%
  filter(男女 == "女のみの学校" | 男女 == "男のみの学校") %>%
  # ggplot でグラフを描く
  ggplot(aes(x = 年, y = 学校数, colour = 男女)) + 
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1990, 2015, by=5), minor_breaks = 1986:2016) +
  scale_y_continuous(breaks = seq(0, 900, by=100), 
                     minor_breaks = seq(0, 900, by=10)) +
  theme_bw() +
  ggtitle("日本における男子のみ・女子のみの高校（通信制除く）の数") + 
  ylab("学校数")

