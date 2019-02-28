best<-function(state, outcome){
        data<-read.csv('/Users/zeinameng/R/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = 'character')
        #check that state and outcome are valid
        if (state %in% state.abb && outcome %in% c('heart attack', 'heart failure','pneumonia')){
                if (outcome=='heart attack'){
                        column=11
                }
                if (outcome=='heart failure'){
                        column=17
                }
                if (outcome=='pneumonia'){
                        column=23
                }
                #read outcome data
                outcome<-data[data$State==state, c(2,column)]
                outcome[,2]<-as.numeric(outcome[,2])
                newdata<-outcome[complete.cases(outcome[,2]),]
                sort1<-newdata[order(newdata[,2],newdata[,1]), ]
                #return hospital name
                head(sort1[[1]],1)
        }
        #error message for invalid arguments
        else{
        if (state %in% state.abb){
        stop('invalid outcome')}
                else{
                if(outcome %in% c('heart attack', 'heart failure','pneumonia')){
                stop('invalid state')}
                      else{stop('invalid state and outcome')}          
                     }
        }              
}
        
                              