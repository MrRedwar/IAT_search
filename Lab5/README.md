# Исследование информации о состоянии беспроводных сетей
mironopavel@yandex.ru

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.

2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.

3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных

4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Программное обеспечение Windows 10 Pro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  Исходные данные

## План

1.  Подготовить данные  
2.  выполнить задание

## Шаги

1 Импортируйте данные. 2 Привести датасеты в вид “аккуратных данных”,
преобразовать типы столбцов в соответствии с типом данных

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
library(stringr)
library(readr)

lines <- readLines("P2_wifi_data.csv")
header_lines <- grep("BSSID, First time seen", lines)
client_header_line <- grep("Station MAC, First time seen", lines)

ap_data <- read_csv("P2_wifi_data.csv", skip = header_lines[1] - 1, 
                    col_names = c("BSSID", "First_time_seen", "Last_time_seen", 
                                  "channel", "Speed", "Privacy", "Cipher", 
                                  "Authentication", "Power", "beacons", 
                                  "IV", "LAN_IP", "ID_length", "ESSID", "Key"),
                    col_types = cols(
                      BSSID = col_character(),
                      First_time_seen = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                      Last_time_seen = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                      channel = col_integer(),
                      Speed = col_integer(),
                      Privacy = col_character(),
                      Cipher = col_character(),
                      Authentication = col_character(),
                      Power = col_integer(),
                      beacons = col_integer(),
                      IV = col_integer(),
                      LAN_IP = col_character(),
                      ID_length = col_integer(),
                      ESSID = col_character(),
                      Key = col_character()
                    ))
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

``` r
client_data <- read_csv("P2_wifi_data.csv", skip = client_header_line - 1,
                        col_names = c("Station_MAC", "First_time_seen", "Last_time_seen",
                                      "Power", "packets", "BSSID", "Probed_ESSIDs"),
                        col_types = cols(
                          Station_MAC = col_character(),
                          First_time_seen = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                          Last_time_seen = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                          Power = col_integer(),
                          packets = col_integer(),
                          BSSID = col_character(),
                          Probed_ESSIDs = col_character()
                        ))
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

``` r
ap_data_clean <- ap_data %>%
  filter(!is.na(First_time_seen), !is.na(Last_time_seen)) %>%
  mutate(ESSID = ifelse(ESSID == "<length: 0>", "Hidden", ESSID)) %>%
  mutate(session_duration = as.numeric(difftime(Last_time_seen, First_time_seen, units = "mins")))

client_data_clean <- client_data %>%
  filter(!is.na(First_time_seen), !is.na(Last_time_seen), 
         BSSID != "(not associated)") %>%
  mutate(session_duration = as.numeric(difftime(Last_time_seen, First_time_seen, units = "mins")))
```

3 Просмотрите общую структуру данных с помощью функции glimpse()

``` r
glimpse(ap_data_clean)
```

    Rows: 12,248
    Columns: 16
    $ BSSID            <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B…
    $ First_time_seen  <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28…
    $ Last_time_seen   <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28…
    $ channel          <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6,…
    $ Speed            <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180…
    $ Privacy          <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2"…
    $ Cipher           <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "…
    $ Authentication   <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", …
    $ Power            <int> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42…
    $ beacons          <int> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 1…
    $ IV               <int> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0,…
    $ LAN_IP           <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0…
    $ ID_length        <int> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, …
    $ ESSID            <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "…
    $ Key              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ session_duration <dbl> 157.78333, 162.15000, 160.46667, 110.96667, 77.26667,…

``` r
glimpse(client_data_clean)
```

    Rows: 186
    Columns: 8
    $ Station_MAC      <chr> "CA:66:3B:8F:56:DD", "5C:3A:45:9E:1A:7B", "C0:E4:34:D…
    $ First_time_seen  <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28…
    $ Last_time_seen   <dttm> 2023-07-28 10:59:44, 2023-07-28 11:51:54, 2023-07-28…
    $ Power            <int> -33, -39, -61, -31, -71, -74, -65, -65, -1, -37, -48,…
    $ packets          <int> 858, 432, 958, 163, 3, 115, 437, 77, 71, 125, 122, 15…
    $ BSSID            <chr> "BE:F1:71:D5:17:8B", "BE:F1:71:D6:10:D7", "BE:F1:71:D…
    $ Probed_ESSIDs    <chr> "C322U13 3965", "C322U21 0566", "C322U13 3965", "C322…
    $ session_duration <dbl> 106.683333, 158.850000, 160.216667, 157.733333, 6.916…

4 Определить небезопасные точки доступа (без шифрования – OPN)

``` r
unsafe_aps <- ap_data_clean %>%
  filter(Privacy == "OPN") %>%
  select(BSSID, ESSID, channel, Power, session_duration) %>%
  arrange(desc(session_duration))

glimpse(unsafe_aps)
```

    Rows: 42
    Columns: 5
    $ BSSID            <chr> "00:25:00:FF:94:73", "E8:28:C1:DD:04:52", "E8:28:C1:D…
    $ ESSID            <chr> NA, "MIREA_HOTSPOT", "MIREA_HOTSPOT", "MIREA_GUESTS",…
    $ channel          <int> 44, 11, 6, 6, 6, 6, 1, 52, 52, 48, 1, 11, 48, 1, 6, 5…
    $ Power            <int> -1, -67, -63, -63, -63, -1, -69, -84, -83, -88, -71, …
    $ session_duration <dbl> 163.25000, 162.93333, 162.58333, 162.10000, 162.08333…

5 Определить производителя для каждого обнаруженного устройства

``` r
library(dplyr)
library(readr)
library(stringr)
library(httr)
library(tibble)
load_oui_database <- function() {
  
  manuf_url <- "https://gitlab.com/wireshark/wireshark/-/raw/release-4.0/manuf"
  
  
  manuf_lines <- read_lines(manuf_url)
  
  
  oui_data <- tibble(line = manuf_lines) %>%
    filter(!str_detect(line, "^#"),
           !str_detect(line, "^$"),
           str_detect(line, "[0-9A-Fa-f]")) %>%
    mutate(
      
      parts = str_split_fixed(line, "\\s+", 3),
      prefix_raw = parts[,1],
      vendor = parts[,2]
    ) %>%
    select(prefix_raw, vendor) %>%
    mutate(
      
      prefix_clean = str_replace_all(prefix_raw, "[:.-]", "") %>% toupper(),
      
      oui = str_sub(prefix_clean, 1, 6)
    ) %>%
    
    distinct(oui, .keep_all = TRUE) %>%
    select(oui, vendor)
  
  return(oui_data)
}


get_oui_vendor <- function(mac_addresses, oui_db) {
 
  clean_mac <- mac_addresses %>%
    str_replace_all("[:.-]", "") %>%
    toupper()
  

  oui <- str_sub(clean_mac, 1, 6)
  
 
  vendor_info <- oui_db$vendor[match(oui, oui_db$oui)]
  
  
  vendor_info[is.na(vendor_info)] <- "Unknown"
  
  return(vendor_info)
}

oui_database <- load_oui_database()

ap_data_clean <- ap_data_clean %>%
  mutate(Manufacturer = get_oui_vendor(BSSID, oui_database))

client_data_clean <- client_data_clean %>%
  mutate(Manufacturer = get_oui_vendor(Station_MAC, oui_database))



glimpse(ap_data_clean)
```

    Rows: 12,248
    Columns: 17
    $ BSSID            <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B…
    $ First_time_seen  <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28…
    $ Last_time_seen   <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28…
    $ channel          <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6,…
    $ Speed            <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180…
    $ Privacy          <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2"…
    $ Cipher           <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "…
    $ Authentication   <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", …
    $ Power            <int> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42…
    $ beacons          <int> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 1…
    $ IV               <int> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0,…
    $ LAN_IP           <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0…
    $ ID_length        <int> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, …
    $ ESSID            <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "…
    $ Key              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ session_duration <dbl> 157.78333, 162.15000, 160.46667, 110.96667, 77.26667,…
    $ Manufacturer     <chr> "Unknown", "Unknown", "Unknown", "Unknown", "Unknown"…

6 Выявить устройства, использующие последнюю версию протокола шифрования
WPA3, и названия точек доступа, реализованных на этих устройствах

``` r
wpa3_ap <- ap_data_clean %>%
  filter(str_detect(Privacy, "WPA3") | str_detect(Authentication, "WPA3")) %>%
  select(BSSID, ESSID, Privacy, Authentication)

glimpse(wpa3_ap)
```

    Rows: 8
    Columns: 4
    $ BSSID          <chr> "26:20:53:0C:98:E8", "A2:FE:FF:B8:9B:C9", "96:FF:FC:91:…
    $ ESSID          <chr> NA, "Christie’s", NA, "iPhone (Анастасия)", "iPhone (Ан…
    $ Privacy        <chr> "WPA3 WPA2", "WPA3 WPA2", "WPA3 WPA2", "WPA3 WPA2", "WP…
    $ Authentication <chr> "SAE PSK", "SAE PSK", "SAE PSK", "SAE PSK", "SAE PSK", …

7 Отсортировать точки доступа по интервалу времени, в течение которого
они находились на связи, по убыванию.

``` r
ap_sessions <- ap_data_clean %>%
  arrange(BSSID, First_time_seen) %>%
  group_by(BSSID) %>%
  mutate(
    time_gap = as.numeric(difftime(First_time_seen, lag(Last_time_seen), units = "mins")),
    new_session = time_gap > 45 | is.na(time_gap)
  ) %>%
  mutate(session_id = cumsum(new_session)) %>%
  group_by(BSSID, session_id) %>%
  summarise(
    ESSID = first(ESSID),
    First_time_seen = min(First_time_seen),
    Last_time_seen = max(Last_time_seen),
    Total_duration = as.numeric(difftime(max(Last_time_seen), min(First_time_seen), units = "mins")),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_duration))

glimpse(ap_sessions)
```

    Rows: 12,240
    Columns: 6
    $ BSSID           <chr> "00:25:00:FF:94:73", "10:51:07:CB:33:BF", "00:95:69:E7…
    $ session_id      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    $ ESSID           <chr> NA, NA, NA, NA, NA, NA, NA, "MIREA_HOTSPOT", NA, "MIRE…
    $ First_time_seen <dttm> 2023-07-28 09:13:06, 2023-07-28 09:13:13, 2023-07-28 …
    $ Last_time_seen  <dttm> 2023-07-28 11:56:21, 2023-07-28 11:56:17, 2023-07-28 …
    $ Total_duration  <dbl> 163.2500, 163.0667, 163.0333, 163.0333, 163.0167, 162.…

8 Обнаружить топ-10 самых быстрых точек доступа.

``` r
top10_speed <- ap_data_clean %>%
  arrange(desc(Speed)) %>%
  select(BSSID, ESSID, Speed, Privacy) %>%
  head(10)

glimpse(top10_speed)
```

    Rows: 10
    Columns: 4
    $ BSSID   <chr> "00:95:69:E7:7D:21", "00:95:69:E7:7C:ED", "00:95:69:E7:7F:35",…
    $ ESSID   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
    $ Speed   <int> 8171, 4096, 2245, 2143, 1062, 1037, 958, 911, 866, 866
    $ Privacy <chr> "(not associated)", "(not associated)", "(not associated)", "E…

9 Отсортировать точки доступа по частоте отправки запросов (beacons) в
единицу времени по их убыванию

``` r
ap_beacon_rate <- ap_data_clean %>%
  mutate(
    beacon_rate = ifelse(session_duration > 0, beacons / session_duration, NA)
  ) %>%
  filter(!is.na(beacon_rate)) %>%
  arrange(desc(beacon_rate)) %>%
  select(BSSID, ESSID, beacons, session_duration, beacon_rate)

glimpse(ap_beacon_rate)
```

    Rows: 124
    Columns: 5
    $ BSSID            <chr> "F2:30:AB:E9:03:ED", "B2:CF:C0:00:4A:60", "3A:DA:00:F…
    $ ESSID            <chr> "iPhone (Uliana)", "Михаил's Galaxy M32", "iPhone XS …
    $ beacons          <int> 6, 4, 5, 1, 1, 1, 5, 1647, 1, 704, 1251, 1390, 647, 6…
    $ session_duration <dbl> 0.11666667, 0.08333333, 0.15000000, 0.03333333, 0.033…
    $ beacon_rate      <dbl> 51.428571, 48.000000, 33.333333, 30.000000, 30.000000…

10 Определить производителя для каждого обнаруженного устройства

``` r
client_data_clean <- client_data_clean %>%
  mutate(Manufacturer = get_oui_vendor(Station_MAC, oui_database))

glimpse(client_data_clean)
```

    Rows: 186
    Columns: 9
    $ Station_MAC      <chr> "CA:66:3B:8F:56:DD", "5C:3A:45:9E:1A:7B", "C0:E4:34:D…
    $ First_time_seen  <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28…
    $ Last_time_seen   <dttm> 2023-07-28 10:59:44, 2023-07-28 11:51:54, 2023-07-28…
    $ Power            <int> -33, -39, -61, -31, -71, -74, -65, -65, -1, -37, -48,…
    $ packets          <int> 858, 432, 958, 163, 3, 115, 437, 77, 71, 125, 122, 15…
    $ BSSID            <chr> "BE:F1:71:D5:17:8B", "BE:F1:71:D6:10:D7", "BE:F1:71:D…
    $ Probed_ESSIDs    <chr> "C322U13 3965", "C322U21 0566", "C322U13 3965", "C322…
    $ session_duration <dbl> 106.683333, 158.850000, 160.216667, 157.733333, 6.916…
    $ Manufacturer     <chr> "Unknown", "Chongqin", "AzureWav", "IntelCor", "Liteo…

11 Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
is_randomized_mac <- function(mac) {
  if(is.na(mac) || mac == "") return(NA)
  first_byte <- as.hexmode(substr(mac, 1, 2))
  return(bitwAnd(first_byte, 0x02) == 0x02)
}

client_data_clean <- client_data_clean %>%
  mutate(
    is_randomized = sapply(Station_MAC, is_randomized_mac)
  )

non_random_clients <- client_data_clean %>%
  filter(!is_randomized) %>%
  select(Station_MAC, Manufacturer, Power, packets)

glimpse(non_random_clients)
```

    Rows: 59
    Columns: 4
    $ Station_MAC  <chr> "5C:3A:45:9E:1A:7B", "C0:E4:34:D8:E7:E5", "68:54:5A:40:35…
    $ Manufacturer <chr> "Chongqin", "AzureWav", "IntelCor", "LiteonTe", "IntelCor…
    $ Power        <int> -39, -61, -31, -71, -1, -37, -48, -37, -65, -29, -43, -59…
    $ packets      <int> 432, 958, 163, 3, 71, 125, 122, 156, 117, 240, 76, 580, 4…

12 Кластеризовать запросы от устройств к точкам доступа по их именам.
Определить время появления устройства в зоне радиовидимости и время
выхода его из нее.

``` r
 client_clusters <- client_data_clean %>%
  filter(!is.na(Probed_ESSIDs), Probed_ESSIDs != "Not Probed") %>%
  group_by(Probed_ESSIDs) %>%
  summarise(
    device_count = n_distinct(Station_MAC),
    first_appearance = min(First_time_seen),
    last_appearance = max(Last_time_seen),
    avg_power = mean(Power, na.rm = TRUE),
    total_packets = sum(packets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(device_count))

glimpse(client_clusters)
```

    Rows: 37
    Columns: 6
    $ Probed_ESSIDs    <chr> "MIREA_HOTSPOT", "GIVC", "MIREA_GUESTS", "Vladimir", …
    $ device_count     <int> 20, 13, 8, 4, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, …
    $ first_appearance <dttm> 2023-07-28 09:14:09, 2023-07-28 09:13:06, 2023-07-28…
    $ last_appearance  <dttm> 2023-07-28 11:56:21, 2023-07-28 11:55:54, 2023-07-28…
    $ avg_power        <dbl> -62.40000, -62.69231, -64.62500, -51.50000, -47.00000…
    $ total_packets    <int> 3180, 2643, 748, 1300, 1816, 985, 148, 80, 100, 56, 3…

13 Оценить стабильность уровня сигнала внури кластера во времени.
Выявить наиболее стабильный кластер

``` r
cluster_stability <- client_data_clean %>%
  filter(!is.na(Probed_ESSIDs), Probed_ESSIDs != "Not Probed") %>%
  group_by(Probed_ESSIDs) %>%
  summarise(
    device_count = n_distinct(Station_MAC),
    mean_power = mean(Power, na.rm = TRUE),
    sd_power = sd(Power, na.rm = TRUE),
    cv_power = ifelse(mean_power != 0, sd_power / abs(mean_power), NA),
    .groups = "drop"
  ) %>%
  filter(!is.na(sd_power)) %>%
  arrange(sd_power) 

glimpse(cluster_stability)
```

    Rows: 11
    Columns: 5
    $ Probed_ESSIDs <chr> "Galaxy A71", "MT_FREE", "Vladimir", "IKB", "GIVC", "Red…
    $ device_count  <int> 2, 2, 4, 2, 13, 2, 20, 8, 2, 2, 2
    $ mean_power    <dbl> -48.50000, -68.00000, -51.50000, -56.00000, -62.69231, -…
    $ sd_power      <dbl> 0.7071068, 1.4142136, 4.1231056, 4.2426407, 5.2183110, 8…
    $ cv_power      <dbl> 0.01457952, 0.02079726, 0.08006030, 0.07576144, 0.083236…

## Оценка результата

В результате выполнения мы поняли основы анализа данных

## Вывод

Таким образом, мы научились, используя язык r, скачивать, анализировать
данные, и обрабатывать для добавления новых данных.
