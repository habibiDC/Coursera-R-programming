rankhospital<-function(state, outcome, num='best'){
        #read outcome data
        data<-read.csv('/Users/zeinameng/R/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = 'character')
        #check that state, outcome and num are valid and assign num value if it's 'best'. Will deal with the value "worst' later.
        if (num %in% 'best'){
                num<-1}
        if (state %in% data$State && outcome %in% c('heart attack', 'heart failure','pneumonia') && num<=nrow(data[data$State==state,])||num %in% 'worst'){
                if (outcome=='heart attack'){
                        column=11
                }
                if (outcome=='heart failure'){
                        column=17
                }
                if (outcome=='pneumonia'){
                        column=23
                }
                #filter data with input arguments.return hospital name and specified outcome columns.
                outcome<-data[data$State==state, c(2,column)]
                #convert outcome column data to numeric becaue it was imported as character
                outcome[,2]<-as.numeric(outcome[,2])
                #remove missing data in the outcome column
                newdata<-outcome[complete.cases(outcome[,2]),]
                sort1<-newdata[order(newdata[,2],newdata[,1]), ]
                sort1$rank<-rank(sort1[[2]],ties.method = 'first')
                # test if num is 'worst' and assign the last index in sort1        
                if  (num %in% 'worst'){
                        num<-nrow(sort1)}
                #return hospital name with the specified state, outcome and rank
                head(sort1[sort1$rank==num,1])
                 }
        #error messages for invalid arguments
        else{
                if (num>nrow(data[data$State==state,])){
                        return(NA)}
                else if(outcome %in% c('heart attack', 'heart failure','pneumonia')){
                                stop('invalid state')}
                        else{stop('invalid outcome')}          
                }
}