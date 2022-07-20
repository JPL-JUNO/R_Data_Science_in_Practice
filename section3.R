library(plyr)
library(ggplot2)
library(scales)

# 读取csv文件
dau <- read.csv('section3-dau.csv', 
                header = T,
                stringsAsFactors = F)

dpu <- read.csv('section3-dpu.csv', 
                header = T,
                stringsAsFactors = F)

install <- read.csv('section3-install.csv', 
                header = T,
                stringsAsFactors = F)

dau.install <- merge(dau, install, by = c('user_id', 'app_name'))

dau.install.payment <- merge(dau.install, dpu, 
                             by = c('log_date', 'user_id', 'app_name'),
                             all.x = T)

# 为了确认消费金额为NA是由不消费导致的
head(na.omit(dau.install.payment))

# 将所有非消费记录的消费额设置为0
dau.install.payment$payment[is.na(dau.install.payment$payment)] <- 0

# 增加一列表示月份
dau.install.payment$log_month <- substr(dau.install.payment$log_date, 
                                        1, 
                                        7)

dau.install.payment$install_month <- substr(dau.install.payment$install_date,
                                           1, 
                                           7)
mau.payment <- ddply(dau.install.payment, 
                     .(log_month, user_id, install_month),
                     summarize,
                     payment = sum(payment))
# 增加属性来区分新用户和已有用户
mau.payment$user.type <- ifelse(mau.payment$install_month == mau.payment$log_month,
                                'install',
                                'existing')

mau.payment.summary <- ddply(mau.payment,
                             .(log_month, user.type),
                             summarize,
                             total.payment = sum(payment))


# 数据可视化
ggplot(mau.payment.summary, aes(x = log_month, y = total.payment, fill = user.type))+
  geom_col(position = position_stack(reverse = TRUE))+  # 将stack顺序调整
  scale_y_continuous(label = comma)


ggplot(mau.payment[mau.payment$payment > 0 & mau.payment$user.type == 'install', ],
       aes(x = payment, fill = log_month))+
  geom_histogram(binwidth = 2000, boundary = 0)

