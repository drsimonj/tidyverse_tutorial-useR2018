#' 
#' This script creates the data sets used in the Tidyverse workshop
#' 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(stringr)

# Set seed for reproducible output ----------------------------------------

set.seed(2018)

# Simulation functions ----------------------------------------------------

#' Simulate bookings for a property
#' 
#' @param n_bookings Number of bookings (rows) to create
#' @param max_room_nights Integer of maximum number of room nights
#' @param long_stay_preference Value from 0 to 1 indicating preference for
#'   longer stays. 1 is more preference for long stays
#' @cancel_rate Probability of a booking being cancelled
#' @booker_uniqueness Value (between 0 and 1) to multiply n_bookings to get
#'   number of unique bookers who can be sampled. Closer to 0 means fewer unique
#'   bookers.
#'   
#' @examples 
#' 
#' x <- simulate_bookings(100, rev(exp(seq_len(7))), .2)
#' x
#' x %>% count(booker_id, sort = TRUE)
#' x %>% count(status, sort = TRUE)
#' 
# simulate_bookings <- function(n_bookings, max_room_nights, long_stay_preference, cancel_rate, booker_uniqueness = 1) {
#   
#   room_night_probs <- rev(exp(seq_len(max_room_nights))) ^ long_stay_preference
#   
#   data_frame(
#     booker_id   = sample(floor(n_bookings * booker_uniqueness), size = n_bookings, replace = TRUE),
#     room_nights = sample(length(room_night_probs), size = n_bookings, prob = room_night_probs, replace = TRUE),
#     status      = sample(c("stayed", "cancelled"), size = n_bookings, prob = c(1 - cancel_rate, cancel_rate), replace = TRUE)
#   )
# }


simulate_bookings <- function(n, booker_uniqueness, cancel_prob, room_night_probs, checkin_day_probs, property_probs, business_prob) {
  data_frame(
    booker_id     = sample(floor(n * booker_uniqueness), size = n, replace = TRUE),
    checkin_day   = sample(names(checkin_day_probs), prob = checkin_day_probs, size = n, replace = TRUE),
    status        = sample(c("cancelled", "stayed"), prob = c(cancel_prob, 1 - cancel_prob), size = n, replace = TRUE),
    room_nights   = sample(length(room_night_probs), prob = room_night_probs,  size = n, replace = TRUE),
    property_type = sample(names(property_probs),    prob = property_probs,    size = n, replace = TRUE),
    for_business = sample(c(TRUE, FALSE), prob = c(business_prob, 1 - business_prob), size = n, replace = TRUE)
  )
}

simulate_tourists <- function(n) {
  booker_uniqueness   <- 1
  room_night_probs  <- c(.3, .8, .8, .7, .5, .3, .2, .1, .05, .01, .001, .001) #rev(exp(seq_len(10)))
  cancel_prob       <- 0.3
  checkin_day_probs <- c(mon = .2, tue = .2, wed = .3, thu = .4, fri = .8, sat = .7, sun = .6)
  property_probs    <- c(Hotel = .5, Apartment = .5)
  business_prob     <- 0.05  # As in they checked "Travelling for business" (which can have errors)
  
  simulate_bookings(n, booker_uniqueness, cancel_prob, room_night_probs, checkin_day_probs, property_probs, business_prob) %>% 
    mutate(booker_id = paste("tourist", booker_id, sep = "_"))
}

simulate_business <- function(n) {
  booker_uniqueness <- .6
  room_night_probs  <- c(.9, .8, .5, .3, .2, .1, .1, .01, .01, .001, .001, .0001) #rev(exp(seq_len(10)))
  cancel_prob       <- 0.1
  checkin_day_probs <- c(mon = .8, tue = .7, wed = .6, thu = .7, fri = .2, sat = .1, sun = .4)
  property_probs    <- c(Hotel = .8, Apartment = .2)
  business_prob     <- 0.80  # As in they checked "Travelling for business" (which can have errors)
  
  simulate_bookings(n, booker_uniqueness, cancel_prob, room_night_probs, checkin_day_probs, property_probs, business_prob) %>% 
    mutate(booker_id = paste("business", booker_id, sep = "_"))
}

#' Creates a random selection of facilities based on property type
create_facilities <- function(property_type) {
  pools <- list(
    Hotel = c("free wifi", "breakfast", "airport shuttle", "on-site restaurant", "pool", "garden", "bbq", "spa", "parking"),
    Apartment = c("kitchen", "free wifi", "laundry", "parking", "pool", "bbq", "garden", "game console", "breakfast")
  )
  
  pool <- pools[[property_type]]
  
  probs <- rev(seq_along(pool))  # As integers (because they get rescaled)
  n     <- sample(probs, size = 1, prob = rev(probs))
  
  facilities <- sample(pool, size = n, prob = probs)
  paste(facilities, collapse = ",")
}

simulate_city <- function(destination, n_bookings, p_tourists, property_uniqueness) {
  n_tourists <- floor(n_bookings * p_tourists)
  n_business <- n_bookings - n_tourists
  
  # Generate all bookings
  bookings <- bind_rows(
    simulate_tourists(n_tourists),
    simulate_business(n_business)
  ) %>% 
    mutate(destination = destination)
  
  # Add property IDs (uniqueness adjusted down for hotels and up for apartments)
  bookings <- bookings %>% 
    group_by(property_type) %>% 
    mutate(property_id = paste0(property_type, "_",
                                sample(floor(n() * property_uniqueness * ifelse(property_type[1] == "Hotel", .5, 3)), 
                                       size = n(), replace = TRUE))) %>% 
    ungroup()
  
  # Add number of rooms per property
  bookings <- bookings %>% 
    group_by(property_id) %>% 
    mutate(nr_rooms = ceiling(n() * median(room_nights) * runif(1, min = .4, max = .6))) %>% 
    ungroup()
  
  # Add price per night (will need to be normalized across all cities)
  bookings <- bookings %>% 
    group_by(property_id) %>% 
    mutate(price_per_night = rnorm(n(), mean = rnorm(1, mean = 80, sd = 20), sd = 20) *
             if_else(property_type[1] == "Apartment", .8, 1) *
             if_else(checkin_day %in% c("Friday", "Saturday"), 1.7, 1) *
             (1 / log(room_nights + 1))) %>%
    ungroup()
  
  # Add facilities
  bookings <- bookings %>% 
    group_by(property_id) %>% 
    mutate(facilities = create_facilities(property_type[1])) %>% 
    ungroup()
  
  # Add review scores - need to be combination of property (some are better than
  # others; related to number of bookings) and match with person
  # Also add price!
  bookings <- bookings %>%
    group_by(property_id) %>%
    mutate(n = n(),
           is_tourist = str_detect(booker_id, "tourist"),
           p_tourists = mean(is_tourist),
           property_baseline = if_else(n == 1, 1, n * var(is_tourist)),
           review_score = property_baseline * (is_tourist - 0.5 + p_tourists) + .01 * price_per_night + rnorm(n())) %>%
    ungroup() %>%
    mutate(review_score = log(review_score - min(review_score) + 1)) %>%
    mutate(review_score = numvert::num_rescale(review_score, new_min = 1, new_max = 10)) %>%
    mutate(review_score = if_else(status == "cancelled", NA_real_, review_score)) %>%
    select(-n, -is_tourist, -p_tourists, -property_baseline)

  bookings 
}

# Create data for cities --------------------------------------------------

city_settings <- tribble(
  ~destination, ~n_bookings, ~p_tourists, ~property_uniqueness,
    "Brisbane",        1000,         0.5,                  0.1,
   "Amsterdam",        5000,         0.8,                  0.6,
       "Tokyo",        4000,         0.3,                  0.5
)

#' Simulate data
#' Hash booker and property ids
#' Rescale all prices
#' Randomly set review scores to missing (20% chance)
d <- pmap_dfr(city_settings, simulate_city) %>% 
  mutate(booker_id = paste(destination, booker_id, sep = "_") %>% map_chr(digest::sha1),
         property_id = paste(destination, property_id, sep = "_") %>% map_chr(digest::sha1) %>% as.factor() %>% as.integer() %>% {. + 1000L}) %>% 
  mutate(price_per_night = numvert::num_rescale(price_per_night, new_min = 30, new_max = 180)^1.08,
         review_score = if_else(runif(n()) < .2, NA_real_, review_score))

# Split and order data ----------------------------------------------------

bookings <- d %>% 
  select(booker_id, property_id, room_nights, price_per_night, checkin_day, for_business, status, review_score)

# And set 5% of nr_rooms to missing
properties <- d %>% 
  select(property_id, destination, property_type, nr_rooms, facilities) %>% 
  distinct() %>% 
  mutate(nr_rooms = if_else(runif(n()) < .05, NA_real_, nr_rooms))

# Write data --------------------------------------------------------------

write_csv(bookings,   "data/bookings.csv")
write_csv(properties, "data/properties.csv")

#' # TESTING/PLAY ------------------------------------------------------------
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' x <- simulate_city(1000, .1, .7)
#' x %>% count(property_id, sort = TRUE)
#' x %>% filter(property_id == "Hotel_1")
#' 
#' x %>% 
#'   ggplot(aes(review_score)) +
#'     geom_histogram()
#' 
#' range(x$review_score)
#' 
#' 
#' x %>% 
#'   group_by(property_id) %>% 
#'   mutate(n = n(),
#'          is_tourist = str_detect(booker_id, "tourist"),
#'          p_tourists = mean(is_tourist),
#'          property_baseline = n * var(is_tourist),
#'          review_score = property_baseline * ((1 - is_tourist) + p_tourists) + rnorm(n())
#'   ) %>% 
#'   ggplot(aes(log(review_score))) +
#'     geom_histogram() +
#'     facet_grid(is_tourist ~ property_type)
#' 
#' 
#' 
#' x %>% count(property_type)
#' 
#' x %>% count(property_type, property_id) %>% count(property_type)
#' 
#' z <- x %>% filter(property_id == "Hotel_1")
#' 
#' z %>% 
#'   mutate(n = n(),
#'          is_tourist = str_detect(booker_id, "tourist"),
#'          p_tourists = mean(is_tourist),
#'          #booker_var = var(is_tourist),
#'          property_baseline = n * var(is_tourist),
#'          review_score = property_baseline * ((1 - is_tourist) + p_tourists) + rnorm(n())
#'          )
#' 
#' review_score <- 0
#' 
#' 
#' 
#' is_tourist <- z$booker_id > 0
#' 
#' # Hotels tend to have higher review scores
#' if (z$property_type == "hotel") review_score <- review_score + .5
#' 
#' if(is_tourist) {
#'   if (review_score <- 0)
#' } else {
#'   
#' }
#' 
#' 
#' simulate_city(1000, .1, .5) %>% 
#'   count(property_type, property_id) %>% 
#'   count(property_type)
#' 
#' simulate_city(1000, .3) %>% 
#'   count(for_business)
#' 
#' 
#' 
#' 
#' 
#' #' IDEAS:
#' #' 
#' #' Let's have destinations that vary in size and relevance for tourists or business (which correlates to proportion of hotels v apartments)
#' #' Tourists have longer stays, fewer repeat bookings, longer book windows, more cancellations, more weekends, more apartments
#' #' Business have shorter stays, more repeat bookings, shorter book windows, less cancellations, more weekdays, more hotels
#' #'
#' #' Can say whether booker ticked "I'm travelling for work"
#' #' 
#' #' Add review score (which is missing when cancelled) and a combination of match between property and booker type (business or not)
#' #' 
#' #' 
#' 
#' 
#' 
#' 
#' # Data settings -----------------------------------------------------------
#' 
#' #' We will simulate Booking.com data of customers making bookings on the website
#' data_settings <- tribble(
#'  ~ destination, ~ n_hotels, ~ n_bookings, ~ stay_rate, ~ pay_at_booking_rate, ~ room_night_prob_adj,
#'     "Brisbane",         5L,        1000L,         0.9,                   0.6,                   1.0,   
#'    "Amsterdam",         6L,        1200L,         0.7,                   0.1,                   0.7,  
#'        "Tokyo",         8L,        1550L,         0.6,                   0.4,                   0.4  
#' )
#' 
#' # Up to how many room nights will people book and with what (unadjusted) probabilities?
#' possible_room_nights <- seq_len(7)
#' room_night_prob      <- rev(exp(possible_room_nights))
#' 
#' # Create data -------------------------------------------------------------
#' 
#' #' Simulate a data frame of booking data
#' sim_booking_data <- function(destination, n_hotels, n_bookings, stay_rate, pay_at_booking_rate, room_night_prob_adj) {
#'   data_frame(
#'     booker_id   = map_chr(paste0(destination, sample(n_bookings, size = n_bookings, replace = TRUE)), digest::sha1),
#'     property_id = map_chr(paste0(sample(n_hotels, size = n_bookings, replace = TRUE)), digest::sha1),
#'     room_nights = sample(possible_room_nights, size = n_bookings, prob = room_night_prob ^ room_night_prob_adj, replace = TRUE),
#'     destination = destination,
#'     status      = sample(c("stayed", "cancelled"), size = n_bookings, prob = c(stay_rate, 1 - stay_rate), replace = TRUE),
#'     payed_at    = sample(c("booking", "property"), size = n_bookings, prob = c(pay_at_booking_rate, 1 - pay_at_booking_rate), replace = TRUE)
#'   ) %>% 
#'     mutate(property_id = as.integer(as.factor(property_id)) + 1000L)
#' }
#' 
#' d <- pmap_dfr(data_settings, sim_booking_data)
#' #d
#' 
#' 
#' # Write data --------------------------------------------------------------
#' 
#' write_csv(d, "data/data_1.csv")
#' 
#' # d %>%
#' #   count(booker_id, sort = TRUE) %>% 
#' #   ggplot(aes(n)) +
#' #     geom_histogram()
#' # 
#' # d %>%
#' #   ggplot(aes(room_nights)) +
#' #   geom_histogram() +
#' #   facet_wrap(~ destination, ncol = 1)
#' # 
#' # d %>%
#' #   group_by(destination) %>% 
#' #   summarise(mean_roomnights = mean(room_nights))
#' # 
#' # # Percentage of OK bookings
#' # d %>%
#' #   count(destination, status) %>% 
#' #   spread(status, n) %>% 
#' #   mutate(p_stays = stayed / (cancelled + stayed)) %>% 
#' #   ggplot(aes(destination, p_stays)) +
#' #     geom_col()
#' # 
#' # # Percentage of OK room nights
#' # d %>% 
#' #   group_by(destination, status) %>% 
#' #   summarise(room_nights = sum(room_nights)) %>% 
#' #   spread(status, room_nights) %>% 
#' #   mutate(p_stays = stayed / (cancelled + stayed)) %>% 
#' #   ggplot(aes(destination, p_stays)) +
#' #   geom_col()
#' # 
#' # 
#' # d %>%
#' #   count(destination, status) %>% 
#' #   spread(status, n) %>% 
#' #   mutate(p_stays = stayed / (cancelled + stayed)) %>% 
#' #   ggplot(aes(destination, p_stays)) +
#' #   geom_col()
#' # 
#' # 
#'  