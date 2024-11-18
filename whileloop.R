df <- as.data.frame(matrix(data=NA, nrow=10, ncol=2))

x=1:10
a=1.5
b=0
y=a*x+b+runif(length(x), -2,2)  

plot(x,y, ylim = c(-10,20))
abline(b,a)

slutcond <- 10
runcond <- 100000
counter <- 0

while(slutcond < runcond) {
  counter = counter+1
  runcond = runcond-1
  print(paste(runcond, "iter:", counter))
}

# hej med dig

# Startværdier
a0 <- 0
b0 <- 0
increment <- 0.15  # Ønsket inkrement for a
max_iter <- 10     # Antal iterationer
distances <- numeric(max_iter + 1)  # Gem distancer her

# While-loopet
i <- 1
convert=T

while (convert == T) {
  # Beregn a og y
  current_a <- a0 + i * increment
  y_current <- current_a * x + b0
  
  # Beregn distancen
  old_distance= distances[i - 1]
  new_distances=sum((y - y_current)^2)
  if(old_distance>new_distances){
    distances[i] = new_distances
  } else{
    print(c("best slope", current_a, "after ", i))
    convert = F
  }
  
  # Tegn linjen
  abline(b0, current_a, col = "green")
  
  # Incrementér tælleren
  i <- i + 1
  Sys.sleep(1)
}

lm(y ~ x)

# Udskriv distancer
print(distances)





# første test

x=1:10
a=1.5
b=0
y=a*x+b+runif(length(x), -2,2)  

plot(x,y, ylim = c(-10,20))
abline(b,a)


a0=0
b0=0
y0=a0*x+b0

dist0=sum((y-y0)^2)
abline(b0,y0)

a1=0+0.1
b1=0
y1=a1*x+b1

dist1=sum((y-y1)^2)
abline(b1,y1)

a2=0+0.3
b2=0
y2=a2*x+b2

dist2=sum((y-y2)^2)
abline(b2,y2)

a3=0+0.5
b3=0
y3=a3*x+b3

dist3=sum((y-y3)^2)
abline(b3,y3)

a4=0+0.7
b4=0
y4=a4*x+b4

dist4=sum((y-y4)^2)
abline(b4,y4)

a5=0+0.9
b5=0
y5=a5*x+b5

dist5=sum((y-y5)^2)
abline(b5,y5)

a6=0+1.1
b6=0
y6=a6*x+b6

dist6=sum((y-y6)^2)
abline(b6,y6)

a7=0+1.3
b7=0
y7=a7*x+b7

dist7=sum((y-y7)^2)
abline(b7,y7)
