def plot_fiddler(s,annotate=false,xoffset=0):
    w = sqrt(4-s^2/4)
    g = (
        polygon2d([(xoffset,0),(xoffset+s,0),(xoffset+s,w),(xoffset,w)],color='black',alpha=0.05)
        + arc((xoffset,w),1,sector=(0,-pi/2),axes=false)
        + line([(xoffset+1,w),(xoffset+s-1,w)])
        + arc((xoffset+s,w),1,sector=(-pi,-pi/2))
        + line([(xoffset,0),(xoffset+s/2-1,0)],color='red')
        + arc((xoffset+s/2,0),1,sector=(0,pi),color='red')
        + line([(xoffset+s/2+1,0),(xoffset+s,0)],color='red')
    )
    if annotate:
        g += (
            line([(xoffset,w),(xoffset+s/2,w),(xoffset+s/2,0),(xoffset,w)],color='gray',linestyle='dashed')
            + text('←s/2→',(xoffset+s/2*4/7,w),color='black',fontsize='30')
            + text('←w→',(xoffset+s/2,w*4/7),rotation=90,color='black',fontsize='30')
            + text('←2→',(xoffset+s/2*3/7,w*4/7),rotation=-180/pi*acos(s/4),color='black',fontsize='30')
        )
    return g

def plot_extra_credit(r,theta):
    def c(cx,cy,color):
        return (circle((cx,cy),1,axes=false,color=color)
                + circle((cx,cy),r,color=color,fill=true,alpha=0.1))
    
    return (sum([sum([c(2*r*i+2*cos(theta)*j,2*sin(theta)*j,color='blue' if is_even(i) else 'red')
                      for i in [max(-1-j,-1)..min(1-j,1)]]) for j in [-1,0,1]])
            + line([(2*r*i+2*cos(theta)*j,2*sin(theta)*j) for (i,j) in [(0,0),(0,1),(-1,1),(-1,0),(0,0)]],
                   linestyle='dotted',color='black')
            + line([(-2*r+2*cos(theta),2*sin(theta)),(cos(theta),sin(theta))],linestyle='dotted',color='black')
            + text(2*r,((-r+2*cos(theta)),2*sin(theta)),color='black',fontsize=15)
            + text(sqrt(4*r^2-1),(-r+3/2*cos(theta),3/2*sin(theta)),color='black',fontsize=15,rotation=-180/pi*cos(theta))
            + text(1,(3/2*cos(theta),3/2*sin(theta)),color='black',fontsize=15,rotation=180/pi*sin(theta))
            + polygon2d([(2*r*i+2*cos(theta)*j,2*sin(theta)*j) for (i,j) in [(0,0),(0,1),(-1,1),(-1,0),(0,0)]],
                   alpha=1/20,color='black')
           )

def plot3d_fiddler(s):
    w = sqrt(4-s^2/4)
    def f1(x,y):
        x0 = round(x/s)*s
        d2 = (x-x0)^2
        return 0 if d2 > 1 else sqrt(1-d2)
    def f2(x,y):
        x0 = round((x-s/2)/s)*s+s/2
        d2 = (x-x0)^2
        return w if d2 > 1 else w - sqrt(1-d2)
    return (plot3d(f1,(-5,5),(-5,5),opacity=1,mesh=true,color='blue',plot_points=[50,5],frame=false) +
            plot3d(f2,(-5,5),(-5,5),opacity=1,mesh=true,color='red',plot_points=[50,5]))

def plot3d_extra_credit():
    dx = 2
    dy = 2*sqrt(2)
    def f1(x,y):
        x0 = round(x/dx)*dx
        y0 = round(y/dy)*dy
        d2 = (x-x0)^2 + (y-y0)^2
        return 0 if d2 > 1 else sqrt(1-d2)
    def f2(x,y):
        x0 = round((x-dx/2)/dx)*dx + dx/2
        y0 = round((y-dy/2)/dy)*dy + dy/2
        d2 = (x-x0)^2 + (y-y0)^2
        return 1 if d2 > 1 else 1 - sqrt(1-d2)
    return (plot3d(f1,(-4,4),(-4,4),opacity=1,mesh=true,color='blue',plot_points=[70,70],frame=false) +
            plot3d(f2,(-4,4),(-4,4),opacity=1,mesh=true,color='red',plot_points=[70,70]))

def exact_result(expr):
    return LatexExpr(latex(expr) + '\\approx' + numerical_approx(expr))

def plot_mtr_1():
    x = 2-1/4*sin(pi/8)
    y = 3/4 - 1/4*cos(pi/8)
    theta = atan(y/(x-1.3))

    return (circle((0,0),1,axes=false)
            + text('→',(-1,0),fontsize=50,color='gray')
            + text('←',(1,0),fontsize=50,color='gray')
            + text('↩︎',(2,1),fontsize=50,color='gray')
            + text('↩︎',(2,-1),fontsize=50,color='gray')
            + arc((2,0.75),0.25,sector=(-3*pi/8,11*pi/8))
            + arc((2,-0.75),0.25,sector=(-11*pi/8,3*pi/8))
            + arc((1.3,0),sqrt((x-1.3)^2+y^2),sector=(-theta,theta))
            + arc((2.7,0),sqrt((x-1.3)^2+y^2),sector=(pi-theta,pi+theta))
    )
