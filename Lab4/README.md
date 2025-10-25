# Исследование метаданных DNS трафика
mironopavel@yandex.ru

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Программное обеспечение Windows 10 Pro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  Програмный пакет dplyr
5.  Данные DNS

## План

1.  Подготовить данные  
2.  Анализ

## Шаги

1 Импортируйте данные DNS

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.1.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(httr)
library(jsonlite)
```


    Присоединяю пакет: 'jsonlite'

    Следующий объект скрыт от 'package:purrr':

        flatten

``` r
dns_data <- read_tsv("dns.log", col_names = FALSE, comment = "#")
```

    Rows: 427935 Columns: 23
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\t"
    chr (13): X2, X3, X5, X7, X9, X10, X11, X12, X13, X14, X15, X21, X22
    dbl  (5): X1, X4, X6, X8, X20
    lgl  (5): X16, X17, X18, X19, X23

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

2 Добавьте пропущенные данные о структуре данных (назначении столбцов)

``` r
colnames(dns_data) <- c("ts", "uid", "id_orig_h", "id_orig_p", "id_resp_h", 
                        "id_resp_p", "proto", "trans_id", "query", "qclass", 
                        "qtype", "rcode", "rcode_name", "AA", "TC", "RD", 
                        "RA", "Z", "answers", "TTLs", "rejected")
if(ncol(dns_data) > 21) {
  dns_data <- dns_data[, 1:21]
}
```

3 Преобразуйте данные в столбцах в нужный формат

``` r
dns_data <- dns_data %>%
  mutate(
    ts = as_datetime(ts),
    id_orig_p = as.integer(id_orig_p),
    id_resp_p = as.integer(id_resp_p)
  )
```

4 Просмотрите общую структуру данных с помощью функции glimpse()

``` r
glimpse(dns_data)
```

    Rows: 427,935
    Columns: 21
    $ ts         <dttm> 2012-03-16 12:30:05, 2012-03-16 12:30:15, 2012-03-16 12:30…
    $ uid        <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bsb…
    $ id_orig_h  <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "192…
    $ id_orig_p  <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 13…
    $ id_resp_h  <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "19…
    $ id_resp_p  <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137,…
    $ proto      <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "ud…
    $ trans_id   <dbl> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 621…
    $ query      <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\…
    $ qclass     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",…
    $ qtype      <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_…
    $ rcode      <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "32",…
    $ rcode_name <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB"…
    $ AA         <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",…
    $ TC         <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ RD         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    $ RA         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    $ Z          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE…
    $ answers    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    $ TTLs       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,…
    $ rejected   <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",…

5 Сколько участников информационного обмена в сети Доброй Организации?

``` r
unique_ips <- unique(c(dns_data$id_orig_h, dns_data$id_resp_h))
cat("\nУчастников обмена:", length(unique_ips), "\n")
```


    Участников обмена: 1359 

6 Какое соотношение участников обмена внутри сети и участников обращений
к внешним ресурсам?

``` r
internal_ips <- dns_data %>%
  filter(str_detect(id_orig_h, "^(10\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.|192\\.168\\.)")) %>%
  distinct(id_orig_h) %>%
  nrow()

external_count <- length(unique_ips) - internal_ips
cat("Соотношение внутренних и внешних участников:", internal_ips, "/", external_count, "\n")
```

    Соотношение внутренних и внешних участников: 205 / 1154 

7 Найдите топ-10 участников сети, проявляющих наибольшую сетевую
активность.

``` r
top_active <- dns_data %>%
  count(id_orig_h, sort = TRUE) %>%
  head(10)
cat("\nТоп активных участников:\n")
```


    Топ активных участников:

``` r
print(top_active)
```

    # A tibble: 10 × 2
       id_orig_h           n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

8 Найдите топ-10 доменов, к которым обращаются пользователи сети и
соответственное количество обращений

``` r
top_domains <- dns_data %>%
  count(query, sort = TRUE) %>%
  head(10)
cat("\nТоп доменов:\n")
```


    Топ доменов:

``` r
print(top_domains)
```

    # A tibble: 10 × 2
       query                                                                       n
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

9 Опеределите базовые статистические характеристики (функция summary())
интервала времени между последовательными обращениями к топ-10 доменам

``` r
top_domain_stats <- dns_data %>%
  filter(query %in% top_domains$query) %>%
  arrange(query, ts) %>%
  group_by(query) %>%
  mutate(time_diff = as.numeric(ts - lag(ts), units = "secs")) %>%
  summarise(
    min = min(time_diff, na.rm = TRUE),
    q1 = quantile(time_diff, 0.25, na.rm = TRUE),
    median = median(time_diff, na.rm = TRUE),
    mean = mean(time_diff, na.rm = TRUE),
    q3 = quantile(time_diff, 0.75, na.rm = TRUE),
    max = max(time_diff, na.rm = TRUE),
    .groups = 'drop'
  )

print(top_domain_stats)
```

    # A tibble: 10 × 7
       query                                    min    q1 median  mean     q3    max
       <chr>                                  <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl>
     1 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00…     0 0.150  0.5   11.2   1.5   52724.
     2 "44.206.168.192.in-addr.arpa"              0 2.09   4     16.0  20.1   49680.
     3 "HPE8AA67"                                 0 0.75   0.75  16.6  25.5   50044.
     4 "ISATAP"                                   0 0.75   0.760 17.5   1.05  51998.
     5 "WPAD"                                     0 0.75   0.75  12.6   1.11  50049.
     6 "safebrowsing.clients.google.com"          0 0      1     10.0   2.01  49952.
     7 "teredo.ipv6.microsoft.com"                0 0      0      2.94  0.510 50388.
     8 "time.apple.com"                           0 0.370  1.76   8.67  4.72  50924.
     9 "tools.google.com"                         0 0      0      8.19  1     50365.
    10 "www.apple.com"                            0 0      1      8.61  3.01  50964.

10 Часто вредоносное программное обеспечение использует DNS канал в
качестве канала управления, периодически отправляя запросы на
подконтрольный злоумышленникам DNS сервер. По периодическим запросам на
один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP
адреса в исследуемом датасете?

``` r
suspicious_activity <- dns_data %>%
  count(id_orig_h, query, sort = TRUE) %>%
  filter(n > 5) %>%
  head(10)

if(nrow(suspicious_activity) > 0) {
  cat("Подозрительные IP с повторяющимися запросами:\n")
  print(suspicious_activity)
} else {
  cat("Нет подозрительной активности\n")
}
```

    Подозрительные IP с повторяющимися запросами:
    # A tibble: 10 × 3
       id_orig_h       query                           n
       <chr>           <chr>                       <int>
     1 10.10.117.210   teredo.ipv6.microsoft.com   27425
     2 192.168.202.93  www.apple.com               10852
     3 10.10.117.210   tools.google.com            10179
     4 192.168.202.83  44.206.168.192.in-addr.arpa  7248
     5 192.168.202.76  HPE8AA67                     6929
     6 192.168.202.93  time.apple.com               6038
     7 192.168.203.63  imap.gmail.com               5543
     8 192.168.202.76  WPAD                         5175
     9 192.168.202.103 api.twitter.com              4163
    10 192.168.202.103 api.facebook.com             4137

## Оценка результата

В результате практической работы мы поняли как анализировать данные DNS
с помощью языка R.

## Вывод

Таким образом, мы научились, используя язык r, скачивать и анализировать
данные DNS.
