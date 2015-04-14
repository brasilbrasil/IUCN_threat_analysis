ID=c(1,2,2,4,5,6,7,8, 8, 10)
geog=c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
a=data.frame(ID, geog)

jnk=duplicated(a$ID)
dup_vals=unique(a$ID[jnk])
a=a[!jnk,]
jnk2=a$ID %in% dup_vals
levels(a[,"geog"])=c(levels(a[,"geog"]),"multi")
a[jnk2,"geog"]="multi"
a
