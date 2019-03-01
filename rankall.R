rankall<-function(outcome, num='best'){
        library(dplyr)
        library(data.table)
        #read outcome data
        data<-read.csv('/Users/zeinameng/R/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = 'character')
        returntable<-data$State

        if (num %in% 'best'){
                num<-1}
        if (outcome %in% c('heart attack', 'heart failure','pneumonia')){
                if (outcome=='heart attack'){
                        column=11
                }
                if (outcome=='heart failure'){
                        column=17
                }
                if (outcome=='pneumonia'){
                        column=23
                }
        #select hospital name, state and the specified outcome columns        
        outcome<-data[, c(2,7,column)]
        #convert outcome column data to numeric becaue it was imported as character
        outcome[,3]<-as.numeric(outcome[,3])
        #remove missing data in the outcome column
        newdata<-outcome[complete.cases(outcome[,3]),]
        #update column name
        colnames(newdata)[[3]]<-'input_outcome'
        #rank outcome value grouped by State
        by_state <- newdata %>% 
                arrange(State, input_outcome, Hospital.Name) %>% 
                group_by(State) %>% 
                mutate(rank=row_number())
        #get factors in State (to create table and NAs on line 42)
        factor<-factor(data$State)
        #fetch the last rows of every group by state if num is 'worst'
        if (num %in% 'worst'){
                #could use worstrank<-ddply(by_state,.(State),tail,1)
                worstrank<-by_state[unlist(tapply(rownames(by_state), by_state$State, tail, 1)), ]
                rankdata<-worstrank[ ,c("Hospital.Name","State")]}
        #fetch rows given rank input
                else{
                        rankdata<-subset(by_state,by_state$rank==num,select=c("Hospital.Name","State"))
                }
        level<-levels(factor)
        #create a dummy table with all state levels
        dummytable<-data.frame('State'=level)
        #join dummy table and rankdata table to create NAs
        table1<-left_join(dummytable,rankdata)
        #create new table
        table2<-data.frame('hospital'=table1$Hospital.Name,'state'=level)
        table2
        }
        #error messages for invalid arguments
        else {
                stop('invalid outcome')}
}