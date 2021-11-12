dev.new()
png('test.png')
dotchart(mtcars$mpg,labels = row.names(mtcars),cex = .7,
         main = "Gas Mileage for Car Models",
         xlab = "Miles Per gallon")
x <- mtcars[order(mtcars$mpg),]        #按照mpg排序
x$cyl <-factor(x$cyl)      #将cyl变成因子数据结构类型
x$color[x$cyl==4] <-"red"   #新建一个color变量，油缸数cyl不同，颜色不同
x$color[x$cyl==6] <-"blue"
x$color[x$cyl==8] <-"darkgreen"
dotchart(x$mpg,        #数据对象
         labels = row.names(x),     #标签
         cex = .7,#字体大小
         groups = x$cyl,      #按照cyl分组
         gcolor = "black",    #分组颜色
         color = x$color,     #数据点颜色
         pch = 19,#点类型
         main = "Gas Mileage for car modes \n grouped by cylinder",    #标题
         xlab = "miles per gallon")     #x轴标签
dev.off()