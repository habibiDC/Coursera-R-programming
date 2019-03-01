best<-function(state, outcome){
        #read outcome data
        data<-read.csv('/Users/zeinameng/R/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = 'character')
        #check that state, outcome are valid
        if (state %in% data$State && outcome %in% c('heart attack', 'heart failure','pneumonia')){
                if (outcome=='heart attack'){
                        column=11
                }
                if (outcome=='heart failure'){
                        column=17
                }
                if (outcome=='pneumonia'){
                        column=23
                }
                #read outcome data with specified state and outcome and return hospital name column too
                outcome<-data[data$State==state, c(2,column)]
                #convert outcome column data to numeric becaue it was imported as character
                outcome[,2]<-as.numeric(outcome[,2])
                #remove missing data in the outcome column
                newdata<-outcome[complete.cases(outcome[,2]),]
                sort1<-newdata[order(newdata[,2],newdata[,1]), ]
                #return hospital name
                head(sort1[[1]],1)
        }
        #error message for invalid arguments
        else{
        if (state %in% state.abb){
                stop('invalid outcome')}
                        else {stop('invalid state')}          
        }              
}
        
                              
