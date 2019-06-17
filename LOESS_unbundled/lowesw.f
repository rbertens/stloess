      subroutine lowesw(res,n,rw,pi)
      integer identi,execnt,i,i1,n,nh
      integer pi(n)
      real cmad,rsmall
      real res(n),rw(n)
      external ehg106
      integer ifloor
      external ifloor
      real r1mach
      external r1mach
      save execnt
      data execnt /0/
c     Identity -> identi
      execnt=execnt+1
c     tranliterated from Devlin's ratfor
c     find median of absolute residuals
      do 3 i1=1,n
         rw(i1)=abs(res(i1))
    3 continue
      do 4 identi=1,n
         pi(identi)=identi
    4 continue
      nh=ifloor(float(n)/2.)+1
c     partial sort to find 6*mad
      call ehg106(1,n,nh,1,rw,pi,n)
      if((n-nh)+1.lt.nh)then
         call ehg106(1,nh-1,nh-1,1,rw,pi,n)
         cmad=3*(rw(pi(nh))+rw(pi(nh-1)))
      else
         cmad=6*rw(pi(nh))
      end if
      rsmall=r1mach(1)
      if(cmad.lt.rsmall)then
         do 5 i1=1,n
            rw(i1)=1
    5    continue
      else
         do 6 i=1,n
            if(cmad*0.999.lt.rw(i))then
               rw(i)=0
            else
               if(cmad*0.001.lt.rw(i))then
                  rw(i)=(1-(rw(i)/cmad)**2)**2
               else
                  rw(i)=1
               end if
            end if
    6    continue
      end if
      return
      end
