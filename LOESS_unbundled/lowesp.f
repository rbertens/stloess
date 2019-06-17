      subroutine lowesp(n,y,yhat,pwgts,rwgts,pi,ytilde)
      integer identi,execnt,i2,i3,i5,m,n
      integer pi(n)
      real c,i1,i4,mad
      real pwgts(n),rwgts(n),y(n),yhat(n),ytilde(n)
      external ehg106
      integer ifloor
      external ifloor
      save execnt
      data execnt /0/
c     Identity -> identi
      execnt=execnt+1
c     median absolute deviation
      do 3 i5=1,n
         ytilde(i5)=abs(y(i5)-yhat(i5))*sqrt(pwgts(i5))
    3 continue
      do 4 identi=1,n
         pi(identi)=identi
    4 continue
      m=ifloor(float(n)/2.)+1
      call ehg106(1,n,m,1,ytilde,pi,n)
      if((n-m)+1.lt.m)then
         call ehg106(1,m-1,m-1,1,ytilde,pi,n)
         mad=(ytilde(pi(m-1))+ytilde(pi(m)))/2
      else
         mad=ytilde(pi(m))
      end if
c     magic constant
      c=(6*mad)**2/5
      do 5 i5=1,n
         ytilde(i5)=1-((y(i5)-yhat(i5))**2*pwgts(i5))/c
    5 continue
      do 6 i5=1,n
         ytilde(i5)=ytilde(i5)*sqrt(rwgts(i5))
    6 continue
      if(n.le.0)then
         i4=0.
      else
         i3=n
         i1=ytilde(i3)
         do 7 i2=i3-1,1,-1
            i1=ytilde(i2)+i1
    7    continue
         i4=i1
      end if
      c=n/i4
c     pseudovalues
      do 8 i5=1,n
         ytilde(i5)=yhat(i5)+(c*rwgts(i5))*(y(i5)-yhat(i5))
    8 continue
      return
      end
