library(NHSRplotthedots)
library(tidyverse)
df <- NHSRdatasets::ae_attendances

orgs <- unique(df$org_code)

f_latestflag <- function(a){
  filter(a,a$`x` == max(a$`x`))
}

f_dimensionspc <- function(dimension_label,
                           .data,
                           dimension_filter,
                           value_field_label,
                           date_field_label,
                           facet_field_label,
                           improvement_direction_label){
  df1 <- select(.data,
           all_of(dimension_label),
           all_of(value_field_label),
           all_of(date_field_label),
           all_of(facet_field_label)) %>%
    rename("dimension"=dimension_label,
           "value"=value_field_label,
           "date"=date_field_label,
           "facet"=facet_field_label) %>%
    filter(dimension==dimension_filter)
 
  spc_object <- ptd_spc(df1,
                        value_field= value,
                        date_field= date,
                        facet_field = facet,
                        improvement_direction = improvement_direction_label)
  spc_flags <- tibble(f_latestflag(spc_object)) %>%
    mutate("dimension"= dimension_filter)
}


test<- f_dimensionspc(.data=df,
                      dimension_label = 'org_code',
                      value_field_label = 'admissions',
                      date_field_label = 'period',
                      facet_field_label = 'type',
                      improvement_direction_label = 'decrease',
                      dimension = 'RX1')


f_spcflagstable <- function(
    dimensions_all,
    .data,
    dimension_label,
    value_field_label,
    date_field_label,
    facet_field_label,
    improvement_direction_label){
  list_rbind(map(dimensions_all,~f_dimensionspc(dimension_filter=.x,
                                                .data=df,
                                                dimension_label = 'org_code',
                                                value_field_label = 'admissions',
                                                date_field_label = 'period',
                                                facet_field_label = 'type',
                                                improvement_direction_label = 'decrease'))) %>%
    rename("latest_period"= x,
           dimension_label = dimension,
           value_field_label=y,
           facet_field_label=f,
           "mean_value"=mean,
           "lower_control_limit"=lpl,
           "upper_control_limit"=upl) %>%
    select(latest_period,
           dimension_label,
           value_field_label,
           facet_field_label,
           point_type)
}

test2 <- f_spcflagstable(dimensions_all=orgs,
                         .data=df,
                         dimension_label = 'org_code',
                         value_field_label = 'admissions',
                         date_field_label = 'period',
                         facet_field_label = 'type',
                         improvement_direction_label = 'decrease')



