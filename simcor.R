simcor = function(x, r,m,s){ r2 = r**2 
ve = 1-r2 
SD = sqrt(ve) 
e = rnorm(length(x), mean=0, sd=SD) 
y = r*x + e  
y1 = m+y*s

return(y1) } 

j=rnorm(50,40,10)
x=scale(j)

f=simcor(x=x,r=0.5,m=120,s=10)
