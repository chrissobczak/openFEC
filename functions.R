##-------------##
# Chris Sobczak #
# Dec 2020 ##--##
##--------##--------------------------##
# https://api.open.fec.gov/developers/ #
##------------------------------------##

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# Save api key from system environemnt
# Must set the key as OPEN_FEC_API_KEY in
# your shell profile
apikey = Sys.getenv("OPEN_FEC_API_KEY")


##-----------------------------------------------------------------------
# SEARCH Functions
##-----------------------------------------------------------------------
# Retrieve all candidate's IDs or committee
# (They are unique for each office sought)
# https://api.open.fec.gov/developers/#/search/get_names_candidates_
# ID is needed to search for other records
# type is default candidate, use 2 for committees
GetIds = function(apikey,candidate_name,type=1){
	types = c('candidates','committees')
	type = types[type]
	baseurl = 'https://api.open.fec.gov/v1'
	endpoint = paste0('/names/',type,'/')
	key_url = paste0('?api_key=',apikey)
	# Parameters have to be & separated
	parameters = paste0(
		# Name (candidate or committee)
		# to search for
		'&q=', candidate_name
	)
	url = paste0(baseurl,endpoint,key_url,parameters)
	raw = GET(url)
	content = content(raw,as = 'text',encoding = 'UTF-8')
	from_json = fromJSON(content)
	if(raw$status_code != '200'){
		cat(paste0('Status Code: ',raw$status_code,'\n'))
	}

	if(length(from_json$results$name) < 1){
		cat('No results found\n')
	}

	return(data.frame(
		candidate=as.character(from_json$results$name),
		id=as.character(from_json$results$id)
	))
}

# getIds returns more than you want usually,
# so chooseIds prompts the user to select
# desired ids
ChooseIds = function(id_dataframe){
	print(id_dataframe)
	ids = readline(prompt="Select IDs to use (space separated): ")
	ids = as.numeric(unlist(str_split(ids,'\\s')))
	return(as.character(id_dataframe[ids,'id']))
}
#------------------------------------------------------------------------


# https://api.open.fec.gov/developers/#/committee/get_candidate__candidate_id__committees_
# Get committees associated with candidated id
GetCommittees = function(apikey, candidate_id){
	max_page = Inf
	page=1
	for(page in 1:2){
		baseurl = paste0('https://api.open.fec.gov/v1')
		endpoint = paste0('/candidate/',candidate_id,'/committees/')
		key_url = paste0('?api_key=',apikey)
		# Parameters have to be & separated
		parameters = paste0(
		#	'&year=',
			'&page=',page
		)
		url = paste0(baseurl,endpoint,key_url,parameters)
		raw = GET(url)
		content = content(raw, as = 'text', encoding = 'UTF-8')
		from_json = fromJSON(content)
	
		if(raw$status_code != '200'){
			cat(paste0('Status Code: ',raw$status_code,'\n'))
		}

		if(length(from_json$results$name) < 1){
			cat('No results found\n')
		}
		max_page = as.numeric(from_json$pagination$pages)
		if(max_page <= page){
			return(from_json$results)
		}
	}
}

# Reciepts:
#------------------------------------------------------------------------
# https://api.open.fec.gov/developers/#/receipts
# Paging here does not work ...
# Need to use the last_index and sort_null_only things together
# Loop through using the indeces thing
GetReceipts = function(apikey, committee_id){
	baseurl = paste0('https://api.open.fec.gov/v1')
	endpoint = paste0('/schedules/schedule_a/')
	key_url = paste0('?api_key=',apikey)
	max_index = 100
	index = 0
	while(index < max_index){
		# Parameters have to be & separated
		parameters = paste0(
					'&per_page=100',
					'&last_index=',as.character(index+100),
					'&sort_null_only=True'
		)
		url = paste0(baseurl,endpoint,key_url,parameters)
		raw = GET(url)
		content = content(raw, as = 'text', encoding = 'UTF-8')
		from_json = fromJSON(content)

		if(ind > 100){
			df = rbind(df,from_json$results)
		}else{
			df = from_json$results
		}
		ind = ind + 100
	}

	# Amount is under contribution_receipt_amount
	return(df)
}



#x = GetId(key, 'biden')
#y = GetCommittees(key, x[2])
#z = GetReceipts(key, y[2])
#
#write.csv(df,quote=FALSE,row.names=FALSE,'df.csv')
