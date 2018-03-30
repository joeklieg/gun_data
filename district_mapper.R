
# state summary incidents vs victims
ggplot(state_summary_pos_gun_type, aes(incidents, victims, color = assault_rifle)) + geom_point() + geom_jitter() + geom_smooth(method = "lm")

ggplot(incident_summary, aes(incidents, victims,)) + geom_point() + geom_smooth(method = "lm")
