library(spotifyr)
#library(ggjoy)
#library(dplyr)
#library(tidyverse)
#library(knitr)
#library(lubridate)

# Set your credentials to System Environment variables that are default arguments to get_spotify_access_token()
Sys.setenv(SPOTIFY_CLIENT_ID = 'b5c4a61095b74ce597f886697704ea3c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '6b2271624f0c49a9abb65255b84cb462')
access_token <- get_spotify_access_token() # Pull your access token into R

#TO DO: READ DATA FILE TO RETRIEVE THE URI'S FROM ALL THE TOP-50 PLAYLISTS

country_codes = c("PT","ES","DE")
country_names = c("Portugal","Spain","Germany")
top50_uris = c("37i9dQZEVXbKyJS56d1pgi","37i9dQZEVXbNFJfN1Vw8d9","37i9dQZEVXbJiZcmkrIHGU")
top50df <- data.frame(country_codes,country_names,top50_uris)
names(top50df) = c("CODE","NAME","URI")

possible_features <- c("anceability","energy","key","loudness","mode","speechiness","acousticness","instrumentalness","liveness","valence","tempo")

compare_feature <- function(countries,feature){
	# TO DO: IF STATEMENT HERE TO CHECK IF ANY COUNTRY CODE IS WRONG
	if( !is_empty(countries[duplicated(countries)]) )
		stop("The list from the first argument cannot contain repeated elements! It makes no sense to compare a country with itself :)")
	if( !(feature %in% possible_features) )
		stop("Third argument must be a valid feature! Use the help command to see the list of possible features :)")
	
	playlists <- list()
	i <- 1
	for(country in countries){
		uri_top_playlist <- as.vector( top50df[top50df$CODE==country,]$URI )
		top_songs <- get_playlist( uri_top_playlist , fields=c("name","followers","tracks") , authorization=get_spotify_access_token() )
		songs_id <- top_songs[["tracks"]][["items"]][["track.id"]]
		display_stats(top_songs,length(songs_id))
		playlists[[i]] <- songs_id
		i <- i + 1
	}
	
	avg_features <- data.frame()
	for(playlist in playlists){
		features <- data.frame()
		for(song in playlist){
			features[nrow(features)+1,1:11] <- get_track_audio_features(song)
			#print(features) # comment to run faster
		}
		avg_features[nrow(avg_features)+1,1:11] <- apply(features,2,mean)
	}
	names(avg_features) <- possible_features
	return(avg_features)
}

display_stats <- function(x,L){
	cat("############### " , x[["name"]] , " ###############\n")
	cat("Number of followers: " , x[["followers"]][["total"]] , "\n")
	avg_seconds <- floor(mean(x[["tracks"]][["items"]][["track.duration_ms"]])/1000)
	cat("Average duration: " , floor(avg_seconds/60) , " min " , avg_seconds%%60 , " sec.\n")
	cat("Percentage of songs with explicit content: " , sum(x[["tracks"]][["items"]][["track.explicit"]])*100/L , "%\n\n")
}

avgs <- compare_feature(list("DE","ES","PT"),"energy")
