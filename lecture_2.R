#必要なパッケージの導入
#install.packages("tidyverse")　#インストールが終わったら，install〜の前に頭文字"#"をつける
#install.packages("readxl")　#インストールが終わったら，install〜の前に頭文字"#"をつける

#パッケージ読み込み
library(tidyverse)
#library(readxl)

#データをRに入れ込む
#データフレーム（DF）と呼ぶ
#書き方は共通して，「df(名前はお好きに) <- read_csv("~~~.cdv")」→enrvironmentを確認

df <- read_csv("2023data.csv")

#データの中身を見てみよう
head(df)

#まずは，"年齢(age)"のデータを見てみよう！
table(df$age) #質的変数的にみる
summary(df$age) #量的変数的にみる

#では，年収（Q2.3）を見てみよう！
table(df$Q2.3)
#割合でみてみよう
prop.table(table(df$Q2.3))

#「99（答えない）」人を分析上から消しましょう！
#同時に，わかりやすく「変数作成」をしてみましょう．
#tidyverseに慣れるために，ちょっと大変かもしれないけど頑張ってみよう！
# %>% は"command + opition + M"が省略コマンドなので，こちらを使いましょう！（手打ちはミスの原因！）

df <- df %>% 
  mutate(income = 
           if_else(Q2.3 == 99, NA_real_, Q2.3))

prop.table(table(df$income))

# %>% でつないで，一気に見ることができます
df %>% 
  mutate(income = 
           if_else(Q2.3 == 99, NA_real_, Q2.3)) %>% 
  with(prop.table(table(income)))

#よりみやすく（少数桁の調整，100分率表記）
df %>% 
  mutate(income = 
           if_else(Q2.3 == 99, NA_real_, Q2.3)) %>% 
  with(round(prop.table(table(income))*100,1))

#図を作ってみる
#自分でデータフレームを作ってみよう

#個別のデータを入力
prop <- c( 5.9, 8.2, 12.2, 14.4, 12.6, 10.6,  8.5,  7.8,  4.8,  4.3,  8.4,  1.6,  0.8)
income.level <- c("100万未満","100-200万","200-300万","300-400万","400-500万","500-600万",
                  "600-700万","700-800万","800-900万","900-1000万","1000-1500万",
                  "1500-2000万","2000万以上")

#データ合成
prop.income <- tibble(prop, income.level)
prop.income

# "prop.income"を使って図を作ってみる(ggplot2利用)
figure1 <- prop.income %>% 
  mutate(income.level = factor(income.level, 
                               levels = c("100万未満","100-200万","200-300万",
                                          "300-400万","400-500万","500-600万",
                                          "600-700万","700-800万","800-900万",
                                          "900-1000万","1000-1500万","1500-2000万",
                                          "2000万以上"))) %>% 
  ggplot(aes(x = income.level, y = prop)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  labs(x = "所得レベル",　
       y = "割合（%)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#図を表示
figure1  

#図を保存
ggsave("世帯所得.png", figure1, dpi = 300, width = 10, height = 6)


####宿題
#Q4.3_1〜Q4.3_8の8つの内容について，欠損値を適切に処理をして，それぞれの平均値を求めましょう．
#可能な人は，8つの平均値を図（ggplot）にしてみましょう
