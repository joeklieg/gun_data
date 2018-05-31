# vegas details manual input
df_vegas$geolocation <- "36.0919,-115.1751"
df_vegas$congresional_district <- "Congressional District: 1"
df_vegas$assault_weapon <- "Assault weapon"
df_vegas$rifle <- "AR-15"
df_vegas$handgun_general <- "Handgun"
df_vegas$handgun_specific <- "9mm"

df_scrape <- bind_rows(df_add, df_vegas)

write_csv(df_add, "df_scrape_excl_vegas.csv")
write_csv(df_scrape, "df_scrape_incl_vegas.csv")