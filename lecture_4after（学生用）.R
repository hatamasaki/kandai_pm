#いつものデータを入れる
library(tidyverse)
#install.packages("modelsummary")
library(modelsummary)
#install.packages("janitor")
library(janitor)
library(scales)

#いつものデータを入れる
data<-read_csv("2023data.csv")

#教育程度の欠損値処理
data <- data %>% 
  mutate(edu = 
           if_else(Q2.1 == 99, NA_real_, Q2.1)) %>% 
  mutate(edu = factor(edu, labels = c("中卒","高卒", "専門卒", "大卒", "院卒")))

#割合を見る
data %>% 
  tabyl(edu, show_na = FALSE)

#PIDを処理
data <- data %>% 
  mutate(pid=
           case_when(Q4.1 == 1 | Q4.1 == 4 ~ "与党",
                     Q4.1 == 12 ~ "無党派",
                     Q4.1 == 88 | Q4.1 == 99 ~ NA_character_,
                     TRUE ~ "野党")) %>% 
  mutate(pid = factor(pid,levels = c("与党","野党", "無党派")))

#割合を見る
data %>% 
  tabyl(pid, show_na = F)

#クロス表を見てみる
#ベースのやり方
cross.pe <- data %>% 
  with(round(prop.table(table(edu,pid),margin = 1)*100,2))
#確認
cross.pe

#tabylを使ったクロス表
data %>% 
  tabyl(edu,pid, show_na = F) %>%
  adorn_percentages("row")

#教育程度を3区分けしてみよう
data <- data %>% 
  mutate(edu3 =
           case_when(Q2.1 == 1 | Q2.1 == 2 ~ "中高卒",
                     Q2.1 == 3 ~ "専門卒",
                     Q2.1 == 4 | Q2.1 == 5 ~ "大院卒",
                     TRUE ~ NA_character_)) %>% 
  mutate(edu3 = factor(edu3,levels = c("中高卒","専門卒","大院卒")))

table(data$edu3)

#クロス表
edu3_df <- data %>% 
  tabyl(edu3, pid, show_na = F) %>%
  adorn_percentages("row")
#確認
edu3_df

#100倍にして見やすくする（％表記）
edu3_df %>% 
  mutate(与党 = 与党 * 100,
         野党 = 野党 * 100,
         無党派 = 無党派 * 100)


###############################################################################
###############################################################################
###############################################################################
#連続変数（年齢）の統計量
d1 <- data %>%
  summarize(Mean = mean(age, na.rm = TRUE),  # 平均値
            Median =  median(age, na.rm = TRUE),  # 中央値
            SD =  sd(age, na.rm = TRUE),  # 標準偏差
            Min = min(age, na.rm = TRUE),  # 最小値
            Max = max(age, na.rm = TRUE),  # 最大値
            Q1 = quantile(age, 0.25, na.rm = TRUE),  # 第一四分位点
            Q3 = quantile(age, 0.75, na.rm = TRUE),
            n = n(), #サンプルサイズ
            SE = SD/sqrt(n)) #標準誤差
  

d1

#図示してみよう
#ggplot
data %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(bins = 62) +
  ylim(0, 100) +
  labs(x = "年齢", y = "出現頻度") + 
  theme_bw()


#年齢でクロス表を書く
data %>% 
  tabyl(age, pid, show_na = F) %>%
  adorn_percentages("row")

#解決策1：世代ごとに分けてみる
data <- data %>% 
  mutate(generation = 
           case_when(age>=18 & age<30 ~ "20代",
                     age>=30 & age<40 ~ "30代",
                     age>=40 & age<50 ~ "40代",
                     age>=50 & age<60 ~ "50代",
                     age>=60 & age<70 ~ "60代",
                     age>=70 ~ "70代以上"
           ))

#度数分布確認
data %>% 
  tabyl(generation)

#世代と支持政党のクロス
data %>% 
  tabyl(generation, pid, show_na = F) %>%
  adorn_percentages("row")

######################################################################
########################################################
# 自民感情温度
data <- data %>% 
  mutate(LDP_emo = Q4.4_1,
         CDP_emo = Q4.4_2)

#2値（たとえば性別）で平均値比較
#性別変数
data <- data %>% 
  mutate(gender = if_else(sex == 1, "男性", "女性"),
        gender = factor(gender, levels = c("男性","女性")))
        

#性別ごとの情報
data %>% 
  group_by(gender) %>% # genderの値ごとに分けると宣言
  summarize(mean = mean(LDP_emo, na.rm = TRUE),
            sd = sd(LDP_emo, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n))

#3値以上で平均値比較
LDP_emo.df <- data %>% 
  drop_na(pid) %>% 
  group_by(pid) %>% # genderの値ごとに分けると宣言
  summarize(mean = mean(LDP_emo, na.rm = TRUE),
            sd = sd(LDP_emo, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n))

#3群以上で比較（方法は色々ある）
TukeyHSD(aov(data$LDP_emo ~ data$pid))
pairwise.t.test(data$LDP_emo, data$pid, p.adj = "bonf")

#絵を書いてみる（boxplot）
data %>% 
  filter(!is.na(pid)) %>% #NA以外で
  ggplot() +
  geom_boxplot(aes(x=pid, y=LDP_emo))

#絵を書いてみる（一般的なグラフ）
LDP_emo.df %>% 
  filter(!is.na(pid)) %>%
  ggplot() +
  geom_point(aes(x = pid, y = mean),
             size = 3) +
  geom_errorbar(aes(x = pid, y = mean, ymin = mean - se, ymax = mean + se),
                linewidth = 0.75, width = 0.15) +
  geom_text(aes(x = pid, y = mean, label=sprintf("%.2f", mean), hjust = -.5))+
  theme_bw(base_size = 15) + 
  labs(x = "支持政党",
       y = "自民党感情温度の平均値",
       caption = "*エラーバーは±1SEを示す") + 
  ylim(0,100)

#立憲民主党の感情温度
CDP_emo.df <- data %>% 
  drop_na(pid) %>% 
  group_by(pid) %>% # genderの値ごとに分けると宣言
  summarize(mean = mean(CDP_emo, na.rm = TRUE),
            sd = sd(CDP_emo, na.rm = TRUE),
            n = n(),
            se = sd/sqrt(n))

#データフレームの合成
LDP_emo.df <- LDP_emo.df %>% 
  mutate(group = "自民党")

CDP_emo.df <- CDP_emo.df %>% 
  mutate(group = "立憲民主党")

LDP_CDP_emo <- rbind(LDP_emo.df, CDP_emo.df)


#絵を書いてみる（分けてみる）
LDP_CDP_emo.fig <- LDP_CDP_emo %>% 
  filter(!is.na(pid)) %>%
  mutate(group = fct_inorder(group)) %>%
  ggplot() +
  geom_point(aes(x = pid, y = mean),
             size = 3) +
  geom_errorbar(aes(x = pid, y = mean, ymin = mean - se, ymax = mean + se),
                linewidth = 0.75, width = 0.15) +
  geom_text(aes(x = pid, y = mean, label=sprintf("%.2f", mean), hjust = -.4))+
  theme_bw(base_size = 15) + 
  labs(x = "支持政党",
       y = "感情温度の平均値",
       caption = "*エラーバーは±1SEを示す") + 
  facet_wrap(~ group)

LDP_CDP_emo.fig

ggsave("自民立民感情温度.png", LDP_CDP_emo.fig, dpi = 300, width = 12, height = 5)

########################################################
#宿題

#感情温度変数
data <- data %>% 
  mutate(自民 = Q4.4_1,
         立憲 = Q4.4_2,
         共産 = Q4.4_3,
         維新 = Q4.4_4,
         国民民主 = Q4.4_5,
         公明 = Q4.4_6,
         参政 = Q4.4_7)




########################################################
#無党派ダミー
data <- data %>% 
  mutate(pid.dummy = 
           case_when(pid=="与党"|pid=="野党" ~ "支持あり",
                     TRUE ~ "支持なし")) %>% 
  mutate(pid.dummy = factor(pid.dummy,levels=c("支持あり","支持なし")))

#クロス表確認
gender.pid <- data %>% 
  tabyl(gender, pid.dummy) %>% 
  adorn_percentages("row")

gender.pid

#wide→long(tidy)形式に変換
gender.pid.long <- gender.pid %>% 
  pivot_longer(-gender) %>% 
  mutate(gender = factor(gender, levels = c("男性","女性")))

gender.pid.long


#図にしてみる
figure.1 <- gender.pid.long %>% 
  ggplot(aes(gender, value, fill = name)) +
  geom_bar(stat = "identity", position = "fill") + 
  geom_label(gender.pid.long %>% filter(name == "支持あり" & gender == "男性"),
             mapping = aes(label=sprintf("%.2f", value*100)),
             nudge_y = .3, size = 8,
             show.legend = FALSE) + 
  geom_label(gender.pid.long %>% filter(name == "支持あり" & gender == "女性"),
             mapping = aes(label=sprintf("%.2f", value*100)),
             nudge_y = .5, size = 8,
             show.legend = FALSE) + 
  geom_label(gender.pid.long %>% filter(name == "支持なし" & gender == "男性"),
             mapping = aes(label=sprintf("%.2f", value*100)),
             nudge_y = -.2, size = 8,
             show.legend = FALSE) + 
  geom_label(gender.pid.long %>% filter(name == "支持なし" & gender == "女性"),
             mapping = aes(label=sprintf("%.2f", value*100)),
             nudge_y = -.3, size = 8,
             show.legend = FALSE) + 
  scale_y_continuous(labels = percent) + 
  labs(x = "性別",
       y = "％",
       fill = "") + 
  theme_bw(base_size = 14) + 
  theme(legend.position = "bottom")
  
figure.1

ggsave("性別無党派.png",figure.1, dpi=300, width = 9, height = 5)


#カイ二乗検定
chisq.test(data$gender,data$pid.dummy,correct=FALSE)
chisq.test(data$gender,data$pid.dummy,correct=FALSE)$observed

############################################################
#比率の差の検定

#たとえば，朝日新聞世論調査と読売新聞世論調査の内閣支持率のズレ
#c（朝日の支持者数, 読売支持者数），c(朝日調査全体N，読売調査全体N)

#朝日
1436 * 0.35
#読売
1072 * 0.39

prop.test(c(418.08, 502.6), c(1436, 1072))

#朝日2012年12月
1357*0.31

#では，朝日2022年12月と1月の調査を比べると？

#prop.test(c(418.08, <自分で入力>), c(1436, <自分で入力>))

############################################################
#平均値の検定
#平均値の検定の場合，カテゴリ間の対応関係を見極める必要がある（スライド参照）

#2群での比較
data %>% 
  group_by(gender) %>% 
  summarise(mean = mean(LDP_emo,na.rm = T))

#t検定（等分散を仮定した場合）
t.test(data$LDP_emo ~ data$gender, var.equal=T)

#3群以上の検定

#維新感情温度
data <- data %>% 
  mutate(ISHIN_emo = Q4.4_4)

data %>% 
  drop_na(pid) %>% 
  group_by(pid) %>% 
  summarise(自民平均値 = mean(LDP_emo,na.rm = T),
            立民平均値 = mean(CDP_emo,na.rm = T),
            維新平均値 = mean(ISHIN_emo,na.rm = T))

#Tukey法で検定
TukeyHSD(aov(LDP_emo ~ pid, data = data)) #自民感情温度
TukeyHSD(aov(CDP_emo ~ pid, data = data)) #立民感情温度
TukeyHSD(aov(ISHIN_emo ~ pid, data = data)) #維新感情温度

#bonferroni法で検定
pairwise.t.test(data$LDP_emo, data$pid, p.adj = "bonf") #自民感情温度
pairwise.t.test(data$CDP_emo, data$pid, p.adj = "bonf") #立民感情温度
pairwise.t.test(data$ISHIN_emo, data$pid, p.adj = "bonf") #維新感情温度

#3世代ごとに分析してみる
data <- data %>% 
  mutate(sedai = 
           case_when(age >= 18 & age < 40 ~ "若年層",
                     age >= 40 & age < 60 ~ "ミドル層",
                     age >= 60 ~ "シニア層",
           ),
         sedai = factor(sedai, levels = c("若年層","ミドル層","シニア層")))






##############################################################
#単回帰分析
#install.packages("coefplot")
library(coefplot)


#年齢と自民感情温度の関係を見てみよう
data %>% 
  ggplot(aes(x = age, y = LDP_emo)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "年齢", y = "自民党感情温度") +
  scale_x_continuous(breaks = seq(18,79, by=5),limits=c(18,79)) +
  theme_bw(base_size = 14)

#自民党の感情温度に与える年齢の効果を検証
result1 <-  lm(LDP_emo ~ age, data = data) 
summary(result1)
modelsummary(result1)

#ビジュアライズしようぜ！
age.lm <- coefplot(result1, intercept = FALSE, lwdOuter = 1)
age.lm
#ggplotだと簡単に保存できる！
ggsave("回帰分析.png",age.lm, dpi = 100, width = 10, height = 5)

#ggplotでもかける
#データを変換→フィルター→ggplot
broom::tidy(result1, conf.int = TRUE) %>%
  filter(term == "age") %>% 
  ggplot() +
  geom_vline(xintercept = 0, color = "red") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = term)) +
  theme_bw(base_size = 12) + 
  labs(x = "推定値",
       y = "変数")

##############################################################
#重回帰分析
#たとえば，政党支持別に見てみると…？

pidg <- data %>%
  filter(!is.na(pid)) %>% 
  ggplot(aes(x = age, y = LDP_emo)) +
  geom_smooth(method = "lm") +
  facet_wrap(~pid, nrow = 1) + 
  labs(x = "", y = "自民党感情温度") + 
  theme_bw(base_size = 14) +
  theme( legend.position = "none" )
pidg
ggsave("支持政党別.png",pidg,dpi = 300, width = 8, height = 5)


#教育程度ごとには？
edug <- data %>% 
  filter(!is.na(edu3)) %>% 
  ggplot(aes(x = age, y = LDP_emo)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ edu3, nrow = 1) + 
  labs(x = "", y = "自民党感情温度") + 
  theme_bw(base_size = 14) + 
  theme(legend.position = "none" ) 
edug
ggsave("教育程度別.png",edug,dpi = 300, width = 10, height = 5)

#edu（教育程度）の量的変数化
data <- data %>% 
  mutate(edu.n = as.numeric(edu))

#性別ごと
data %>% 
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = age, y = LDP_emo)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ gender, nrow = 1) + 
  labs(x = "", y = "自民党感情温度") + 
  theme_bw(base_size = 14) + 
  theme( legend.position = "none" )


#都市規模ごと
data <- data %>% 
  mutate(citysize.f = if_else(Q2.5 == 99, NA_real_, 5 - Q2.5),
         citysize.f = factor(citysize.f, label =
                             c("町村","一般市","県庁市","東京23区域+政令市")))

data <- data %>% 
  mutate(citysize = if_else(Q2.5 == 99, NA_real_, 5 - Q2.5))
         
data %>% 
  with(table(citysize.f))

data %>% 
  filter(!is.na(citysize.f)) %>% 
  ggplot(aes(x = age, y = 自民)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ citysize.f, nrow = 1) + 
  labs(x = "年齢", y = "自民党感情温度") + 
  theme_bw(base_size = 14) + 
  theme( legend.position = "none" )

#収入
data <- data %>% 
  mutate(income =
           if_else(Q2.3 == 99, NA_real_, Q2.3))

data %>% 
  mutate(income3 =
           case_when(Q2.3 == 99 ~ NA_character_,
                     Q2.3 >= 1 & Q2.3 < 4 ~ "低所得者層",
                     Q2.3 >= 4 & Q2.3 < 8 ~ "中所得者層",
                     Q2.3 >= 8 ~ "高所得者層"
           )) %>% 
  mutate(income3 = factor(income3, 
                          level=c("低所得者層","中所得者層","高所得者層"))) %>% 
  filter(!is.na(income3)) %>% 
  ggplot(aes(x = age, y = 自民)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ income3, nrow = 1) + 
  labs(x = "年齢", y = "自民党感情温度") + 
  theme_bw(base_size = 14) + 
  theme( legend.position = "none" )

#イデオロギー
data <- data %>% 
  mutate(ideology = Q4.2_1)

#重回帰分析
result2 <- data %>% 
  lm(自民 ~ age + gender + edu.n + income + citysize + ideology + pid, data = .)

summary(result2)
modelsummary(result2)


#ビジュアライズしようぜ！2
reg2 <- coefplot(result2, intercept = FALSE, lwdOuter = 1,
                 xlab = "係数の推定値",
                 ylab = "説明変数",
                 title = "",
                 newNames = c(pid無党派 = "支持政党:無党派",
                              pid野党 = "支持政党:野党",
                              age = "年齢",
                              gender女性 = "性別(女性)",
                              edu.n = "教育程度",
                              income = "世帯収入",
                              citysize = "都市規模",
                              ideology = "イデオロギー"),
                 decreasing = TRUE)

reg2
ggsave("coefplot.practice.png",reg2,dpi = 300, width = 8, height = 5)

#ggplotでもっとスマートに
#絵を書くようのデータフレームを準備
coef.practice.plot <- broom::tidy(result2, conf.int = TRUE)
head(coef.practice.plot,10)

#変数名を日本語にしたい
coef.practice.plot <- coef.practice.plot %>% 
  mutate(varname = 
           case_when(term == "age" ~ "年齢",
                     term == "gender女性" ~ "性別（女性）",
                     term == "edu.n" ~ "教育程度",
                     term == "income" ~ "世帯収入",
                     term == "citysize" ~ "都市規模",
                     term == "ideology" ~ "イデオロギー",
                     term == "pid野党" ~ "野党支持",
                     term == "pid無党派" ~ "無党派"
                     ),
         varname = factor(varname, levels = 
                            c("年齢","性別（女性）","教育程度","世帯収入",
                              "都市規模","イデオロギー","野党支持","無党派")))

coef.ggplot1 <- coef.practice.plot %>% 
  filter(!term == "(Intercept)") %>% 
  mutate(varname = fct_reorder(varname, desc(varname))) %>%
  ggplot() +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_pointrange(aes(x = estimate, xmin = conf.low, xmax = conf.high,
                      y = varname)) +
  geom_text(aes(x = estimate, y = varname, label=sprintf("%.2f", estimate), vjust = -1)) +
  theme_bw(base_size = 14) + 
  labs(x = "推定値",
       y = "変数")

coef.ggplot1

ggsave("重回帰分析の図.png",coef.ggplot1, dpi = 300, width = 6, height = 5)

#############################################################
#予測値の計算

#install.packages("marginaleffects")

library(marginaleffects)

##年齢の予測値をプロット

#marginaleffects

pred <- predictions(result2,
                    newdata = datagrid(
                      age = c(18:79),
                      pid = "無党派",
                      gender = "男性"))

pred %>% 
  ggplot(aes(age, estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "年齢", y = "自民党感情温度の予測値",title = " 男性・無党派の場合") + 
  theme_bw(base_size = 14) + 
  scale_x_continuous(breaks = seq(18,79, by=5),limits=c(18,79))
 
##性別ごとの予測値:marginseffect
gender.pred <- predictions(result2,
                          newdata = datagrid(
                            gender =c("男性","女性")))

gender.pred %>% 
  ggplot(aes(gender, estimate)) +
  geom_point(position = position_dodge(.1),
             size = 5) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1),
    width = 0.1, 
    size = 0.5) +
  geom_text(aes(x = gender, y = estimate, label=sprintf("%.2f", estimate)), 
                hjust = -.5, size = 6) +
  labs(x = "性別", y = "自民党感情温度の予測値") + 
  theme_bw(base_size = 12)

##イデオロギーの予測値:marginseffect
ideology.pred <- predictions(result2, 
                            newdata = datagrid(ideology=c(0:10)))

ideology.pred %>% 
  ggplot(aes(ideology, estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  labs(x = "イデオロギー", y = "自民党感情温度の予測値") + 
  theme_bw(base_size = 14) + 
  scale_x_continuous(breaks = seq(0,10, by=1),limits=c(0,10))


##補足1：量的変数と質的変数の交差項を使った予測値（年齢と政党支持）
result.interaction1 <- data %>% 
  lm(LDP_emo ~ age * pid + gender + edu + income + citysize + ideology, data = .)

pred.add <- predictions(result.interaction1,
                      newdata = datagrid(age = c(18:79),
                                         pid = c("与党","野党","無党派")))

pred.add %>% 
  ggplot(aes(age, estimate, color= pid, fill = pid)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1, colour = NA) +
  labs(x = "年齢", y = "自民党感情温度の予測値", color = "支持政党") + 
  guides(fill = F) + 
  scale_x_continuous(breaks = seq(18,79, by=5),limits=c(18,79)) + 
  scale_y_continuous(breaks = seq(30,70, by=5),limits=c(30,70)) + 
  theme_bw(base_size = 14)

##補足2：連続変数同士の交差項を使った有意確認の推定方法（年齢とイデオロギー）
result.interaction2 <- data %>% 
  lm(LDP_emo ~ age * ideology + pid + gender + edu + income + citysize, data = .)
summary(result.interaction2)

#marginaleffectパッケージ内のplot_cme()を使う
plot_cme(result.interaction2, "age", condition = "ideology", draw = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "イデオロギー", y = "自民党感情温度に対する年齢の限界効果") + 
  theme_bw(base_size = 14) + 
  scale_x_continuous(breaks = seq(0,10, by=1),limits=c(0,10))

 
################################################################################
