sites_ids <- data.frame(site = c("Boa Forma", "Capricho", "Guia do Estudante",
                                 "Quatro Rodas", "Saúde", "Superinteressante",
                                 "Veja", "Claudia", "Você.SA"),
                        
                        id = c("128974046", "22763039", "24051908", 
                               "45238368", "128969945", "24051965",
                               "37837306", "59042287", "209350856")
)

#Não vale a pena nem criar um FOR, uma vez que as requisições são canceladas
#pelo Google Analytics

################################################################################

require(RGoogleAnalytics)

# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created 

client.id <- "809494276035-c2lp7fut4vv3nr5asogk2psdpuq6t0hi.apps.googleusercontent.com"
client.secret <- "oBncMK6HgCqyAdJJolLU1TEQ"

# and saved
token <- Auth(client.id,client.secret)

# Save the token object for future sessions
save(token,file="./token_file")

# In future sessions it can be loaded by running load("./token_file")

ValidateToken(token)

#2015-01-02 a 2020-09-20
#p1 2015-01-02 a 2016-01-02
#p2 2016-01-03 a 2017-01-02
#p3 2017-01-03 a 2018-01-02
#p4 2018-01-03 a 2019-01-02
#p5 2019-01-03 a 2020-01-02
#p3 2020-01-04 a 2020-09-20

# Build a list of all the Query Parameters
query.list <- Init(start.date = "2015-01-02",
                   end.date = "2020-09-20",
                   dimensions = "ga:isoYearIsoWeek",
                   metrics = "ga:sessions,ga:users, ga:pageviews, ga:sessionDuration",
                   max.results = 10000,
                   sort = "-ga:isoYearIsoWeek",
                   table.id = "ga:209350856")

# Create the Query Builder object so that the query parameters are validated
ga.query <- QueryBuilder(query.list)

# Extract the data and store it in a data-frame
ga.data <- GetReportData(ga.query, token, split_daywise = T, delay = 0.05)
