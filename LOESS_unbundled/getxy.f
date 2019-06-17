      subroutine getxy(n,d,x,y,w)
      integer n, d, i, j
      real x(n,d), y(n), w(n)
      do 100 i=1,n
         read(5,*,end=110) (x(i,j),j=1,d),y(i),w(i)
100   continue
      return
110   print *, 'unexpected eof  i=',i,' n=',n
      stop
      end
