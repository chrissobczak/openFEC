library(httr)
library(jsonlite)
library(dplyr)

# Save api key from system environemnt
# Must set the key as OPEN_FEC_API_KEY in
# your shell profile
key = Sys.getenv("OPEN_FEC_API_KEY")

# Retrieve Candidate's Id (They are unique for each office sought)
GetId = function(apikey, candidate_name){
	baseurl = paste0('https://api.open.fec.gov/v1')
	endpoint = paste0('/names/candidates/')
	key_url = paste0('?api_key=',apikey)
	# Parameters have to be & separated
	parameters = paste0(
				'&q=', candidate_name
	)
	url = paste0(baseurl,endpoint,key_url,parameters)
	raw = GET(url)
	content = content(raw, as = 'text', encoding = 'UTF-8')
	# Convert to json
	from_json = fromJSON(content)
	print(paste0('Status Code: ', raw$status_code))
	print(from_json$results$name)
	return(from_json$results$id)
}


GetCommittees = function(apikey, candidate_id){
	baseurl = paste0('https://api.open.fec.gov/v1')
	endpoint = paste0('/candidate/',candidate_id,'/committees/')
	key_url = paste0('?api_key=',apikey)
	# Parameters have to be & separated
	parameters = paste0(
				''
	)
	url = paste0(baseurl,endpoint,key_url,parameters)
	raw = GET(url)
	content = content(raw, as = 'text', encoding = 'UTF-8')
	# Convert to json
	from_json = fromJSON(content)
	print(paste0('Status Code: ', raw$status_code))
	print(from_json$results$name)
	return(from_json$results[,c('name','committee_id')])
}


# Paging here does not work ...
# Need to use the last_index and sort_null_only things together
# Loop through using the indeces thing
GetReceipts = function(apikey, committee_id){
	baseurl = paste0('https://api.open.fec.gov/v1')
	endpoint = paste0('/schedules/schedule_a/')
	key_url = paste0('?api_key=',apikey)
	ind = 100
	repeat{
		# Parameters have to be & separated
		parameters = paste0(
					'&per_page=100',
					'&two_year_transaction_period=2020',
					'&last_index=',as.character(ind),
					'&sort_null_only=True'
		)
		url = paste0(baseurl,endpoint,key_url,parameters)
		raw = GET(url)
		content = content(raw, as = 'text', encoding = 'UTF-8')
		# Convert to json
		from_json = fromJSON(content)

		print(paste0('Status Code: ', raw$status_code, ' | Pagination: ', as.character(unlist(from_json$pagination))))
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



x = GetId(key, 'biden')
y = GetCommittees(key, x[2])
z = GetReceipts(key, y[2])

write.csv(df,quote=FALSE,row.names=FALSE,'df.csv')
