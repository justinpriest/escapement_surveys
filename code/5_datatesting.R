source("code/1_dataimport.R")


ktn_index %>%
  group_by(stream_name) %>%
  summarise(na_count = sum(is.na(total_count)))

ktn_index %>%
  summarise(overallmean = mean(total_count, na.rm=TRUE),
            overallsd = sd(total_count, na.rm=TRUE))


ktn_index %>%
  group_by(year) %>%
  summarise(na_count = sum(is.na(total_count)))

ktn_index %>% #filter(stream_name == "Marten River") %>%
  ggplot(aes(x=total_count)) + geom_histogram()

hist(rgamma(1000, 0.8, 4)*4000)
ggplot(data=data.frame(x=(rgamma(1000, 0.8, 4))*4000), aes(x)) + geom_histogram()






ktnsumm <- ktn_index %>%
  group_by(stream_name) %>%
  summarise(meancount = mean(total_count, na.rm = TRUE),
            valcount = sum(!is.na(total_count)),
            stdev = sd(total_count, na.rm = TRUE)) %>%
   mutate(cicalc_lo  = meancount - stdev, 
          cicalc_hi = meancount + stdev)


ktnsumm1 <- ktn_index %>%
  group_by(stream_name) %>%
  do(add_boot_ci_cols(.$total_count))


ktnsumm <- ktnsumm %>% 
  left_join(ktnsumm1, 
            by = c("stream_name" = "stream_name", "valcount" = "ncount", "meancount" = "mean")) %>%
  rename(rawboot_lo = lower_ci,
         rawboot_hi = upper_ci)







####### 
# USE QUASI POISSON
rqpois <- function(n, mu, theta) {
  rnbinom(n = n, mu = mu, size = mu / (theta-1))
}

hist(rqpois(1000, 121, 50))


# The following uses quasipoisson with theta = sd^2 / mu
# Verified that this works by running a glm q pois model, eg:
ktn_index %>% filter(stream_name == "Blossom River") %>%
  glm(total_count ~ 1, data=., family=quasipoisson) %>% summary() # theta = 1692.7
# The theta is ~same as mu=1902, sd=1794, theta=(1794^2)/1902)=1692.1


ktn_index %>% filter(stream_name == "Barrier Creek") %>%
  ggplot(aes(x=total_count)) + geom_histogram()
ggplot(data=data.frame(x=rqpois(100, 121, (115^2)/121)), aes(x)) + geom_histogram() # uses theta = sd^2/mu

ktn_index %>% filter(stream_name == "Blossom River") %>%
  ggplot(aes(x=total_count)) + geom_histogram() + ggtitle("Blossom River")
ggplot(data=data.frame(x=rqpois(50, 1902, (1794^2)/1902)), aes(x)) + geom_histogram() # uses theta = sd^2/mu

ktn_index %>% filter(stream_name == "Marten River") %>%
  ggplot(aes(x=total_count)) + geom_histogram()
ggplot(data=data.frame(x=rqpois(50, 1388, (748^2)/1388)), aes(x)) + geom_histogram() # uses theta = sd^2/mu

ktn_index %>% filter(stream_name == "Humpback Creek") %>%
  ggplot(aes(x=total_count)) + geom_histogram()
ggplot(data=data.frame(x=rqpois(50, 565, (542^2)/565)), aes(x)) + geom_histogram() # uses theta = sd^2/mu





##########################











ktn_index %>%
  filter(stream_name == "Marten River") %>%
  glm(total_count ~ 1, data=., family=quasipoisson) %>%
  summary()


ktn_index %>%
  filter(stream_name == "Blossom River") %>%
  glm(total_count ~ 1, data=., family=quasipoisson) %>%
  summary()
tempmod1 <- ktn_index %>%
  filter(stream_name == "Blossom River") %>% glm(total_count ~ 1, data=., family=quasipoisson) 
predict.glm(tempmod1, newdata = data.frame(x=900), se.fit = TRUE, interval = "prediction")

data.frame(x=rqpois(32, 565, (542^2)/565)) %>%
  do(add_boot_ci_cols(.$x))


exp(confint(glm(rqpois(32, 565, (542^2)/565) ~ 1, family=quasipoisson))) # Humpback

exp(confint(glm(rqpois(34, 1388, (748^2)/1388) ~ 1, family=quasipoisson))) # Marten
exp(confint(glm(rqpois(34, 1388, (748^2)/1388) ~ 1, family=quasipoisson), level = 0.9)) # Marten


data.frame(x=rqpois(1000, 1388, (748^2)/1388)) %>% # MARTEN
  arrange(x) %>%
  mutate(rownum = 1:1000) %>%
  filter(between(rownum, 26, 975)) %>%
  summarise(min(x),
            max(x))


data.frame(x=rqpois(1000, 565, (542^2)/565)) %>% # Humpback
  arrange(x) %>%
  mutate(rownum = 1:1000) %>%
  filter(between(rownum, 26, 975)) %>%
  summarise(min(x),
            max(x))





data.frame(x=rqpois(1000, 1902, (1794^2)/1902)) %>% # Blossom
  arrange(x) %>%
  mutate(rownum = 1:1000) %>%
  filter(between(rownum, 26, 975)) %>%
  summarise(min(x),
            max(x))





hist(rnbinom(1000, mu=1902, size = 2.75)) # Blossom
(1902*2.75) / (1-2.75)^2

hist(rnbinom(1000, mu=565, size = 2.5)) # Humpb
(565*2.5) / (1-2.5)^2

hist(rnbinom(1000, mu=1388, size = 4)) # Marten
(1388*4) / (1-4)^2

hist(rnbinom(1000, mu=121, size = 2.75)) # Barrier
(121*2.75) / (1-2.75)^2


(121*2.75) / (1-2.75)^2 # Barrier
(1902*2.75) / (1-2.75)^2 # Blossom
(356*3) / (1-3)^2 # Carroll
(218*3.25) / (1-3.25)^2 # Choca
(594*3.25) / (1-3.25)^2 # Eulachon
(127*3) / (1-3)^2 # Grant
(166*2.75) / (1-2.75)^2 # Herman
(565*2.5) / (1-2.5)^2 # Humpback
(656*3.5) / (1-3.5)^2 # Indian
(1428*3.25) / (1-3.25)^2 # Keta
(521*3.5) / (1-3.5)^2 # King
(132*2.75) / (1-2.75)^2 # Klahini
(1388*3.5) / (1-3.5)^2 # Marten
(1279*3) / (1-3)^2 # Tombstone



(711*2.37) / (1-2.37)^2

(2.8+(sqrt(2.8^2 - 4)))/2


data.frame(x=rnbinom(1000, mu=1902, size = 2.75)) %>% # Blossom
  arrange(x) %>%
  mutate(rownum = 1:1000) %>%
  filter(between(rownum, 26, 975)) %>%
  summarise(min(x),
            max(x))













ktnsumm <- ktnsumm %>%
  mutate(b = (meancount / stdev)+2,
         nbinomsize = (b + sqrt((b^2) - 4))/2) %>%# Quadratic equation to solve for var=(mu*r)/(1-r)^2
  # mutate(nbinomsize = c(2.75, 2.75, 3, 3.25, 3.25, 3, 
  #                       2.75, 2.5, 3.5, 3.25, 3.5, 2.75, 3.5, 3)) %>%
  rowwise() %>%
  mutate(minnbboot95 = min_nb_boot(n=1000, mu=meancount, size=nbinomsize, alpha = 0.05),
         maxnbboot95 = max_nb_boot(n=1000, mu=meancount, size=nbinomsize, alpha = 0.05),
         minnbboot90 = min_nb_boot(n=1000, mu=meancount, size=nbinomsize, alpha = 0.1),
         maxnbboot90 = max_nb_boot(n=1000, mu=meancount, size=nbinomsize, alpha = 0.1)) %>%
  dplyr::select(-b) %>%
  mutate(qpois_lo = exp(confint(glm(rqpois(valcount, meancount, (stdev^2)/meancount) ~ 1, family=quasipoisson), level = 0.95))[1] %>% as.numeric(),
         qpois_hi = exp(confint(glm(rqpois(valcount, meancount, (stdev^2)/meancount) ~ 1, family=quasipoisson), level = 0.95))[2] %>% as.numeric())




# CALCULATE BOOTSTRAPPED QPOIS
ktnsumm %>%
  group_by(stream_name) %>%
  mutate(temp = (rqpois(valcount, meancount, (stdev^2)/meancount))) %>%
  do(add_boot_ci_cols(as.vector(.$temp)))
tempcicols




ktn_index %>%
  group_by(stream_name) %>%
  do(add_boot_ci_cols(.$total_count))


varnb <- 894^2
(((711 / varnb)+2) + sqrt((((711 / varnb)+2)^2) - 4))/2

ggplot(data=data.frame(x=rnbinom(1000, mu=711, size=(1.03))), aes(x)) + geom_histogram()
ktn_index %>% ggplot(aes(x=total_count)) + geom_histogram()

ktn_index %>%
  group_by(year) %>%
  summarise(anncount = sum(total_count, na.rm = TRUE)) %>% 
  ggplot(aes(x=anncount)) + geom_histogram()


  


(((8650 / 3811)+2) + sqrt((((8650 / 3811)+2)^2) - 4))/2
ggplot(data=data.frame(x=rnbinom(1000, mu=8650, size=(4.02))), aes(x)) + geom_histogram()



# This makes an 
out1 <- data.frame(stream_name=unique(ktnsumm$stream_name))
for(j in 1:14){
  for(k in 2:31){
    out1[j,k] <- rnbinom(1, mu=as.numeric(ktnsumm[j,2]), size=as.numeric(ktnsumm[j,"nbinomsize"]))
  }
}


out1 %>%
  rowwise() %>%
  group_by(stream_name) %>%
  summarise(meansimcount = mean(V2:V31))


out1 %>%
  pivot_longer(-stream_name, values_to = "simcount", names_to = "simrun") %>%
  filter(stream_name == "Marten River") %>%
  ggplot(aes(x=simcount)) + geom_histogram()




###################################################





abovebelow <- ktn_index %>% 
  filter(!is.na(total_count)) %>%
  dplyr::select(year, stream_name, total_count) %>%
  left_join(ktnsumm) %>%
  mutate(abovebelow_sd = case_when(
    total_count >= cicalc_hi ~ "above",
    total_count <= cicalc_lo ~ "below",
    TRUE ~ "within"),
    abovebelow_boot = case_when(
      total_count >= rawboot_hi ~ "above",
      total_count <= rawboot_lo ~ "below",
      TRUE ~ "within"),
    abovebelow_nbin = case_when(
      total_count >= maxnbboot95 ~ "above",
      total_count <= minnbboot95 ~ "below",
      TRUE ~ "within"),
    abovebelow_qpoi = case_when(
      total_count >= qpois_hi ~ "above",
      total_count <= qpois_lo ~ "below",
      TRUE ~ "within")) %>% 
  mutate(stream_name = fct_relevel(stream_name, c("Herman Creek", "Grant Creek",	"Eulachon River", "Klahini River", "Indian Creek", 
                                                  "Barrier Creek", "King Creek", "Choca Creek",	"Carroll Creek", "Blossom River", 
                                                  "Keta River", "Marten River", "Humpback Creek", "Tombstone River"))) %>%
  mutate(surveygroup = ifelse(stream_name %in% c("Carroll Creek", "Blossom River", "Keta River", 
                                                 "Marten River", "Humpback Creek", "Tombstone River"),
                              "South End", "North End"))
  #dplyr::select(year, stream_name, total_count, abovebelow_sd, abovebelow_boot, abovebelow_nbin, abovebelow_qpoi)



abovebelow %>%
  dplyr::select(stream_name, year, abovebelow_sd) %>%
  count(stream_name, abovebelow_sd) %>%
  group_by(stream_name) %>%
  mutate(freq = n / sum(n)) %>% View()

abovebelow %>%
  dplyr::select(stream_name, year, abovebelow_boot) %>%
  count(stream_name, abovebelow_boot) %>%
  group_by(stream_name) %>%
  mutate(freq = n / sum(n)) %>% View()

abovebelow %>%
  dplyr::select(stream_name, year, abovebelow_nbin) %>%
  count(stream_name, abovebelow_nbin) %>%
  group_by(stream_name) %>%
  mutate(freq = n / sum(n))

abovebelow %>%
  dplyr::select(stream_name, year, abovebelow_qpoi) %>%
  count(stream_name, abovebelow_qpoi) %>%
  group_by(stream_name) %>%
  mutate(freq = n / sum(n))









c("Herman Creek", "Grant Creek",	"Eulachon River", "Klahini River", "Indian Creek", 
  "Barrier Creek", "King Creek", "Choca Creek",	"Carroll Creek", "Blossom River", 
  "Keta River", "Marten River", "Humpback Creek", "Tombstone River")



