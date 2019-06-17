      subroutine ehg125(p,nv,v,vhit,nvmax,d,k,t,r,s,f,l,u)
      logical i1,i2,match
      integer d,execnt,h,i,i3,j,k,m,mm,nv,nvmax,p,r,s
      integer f(r,0:1,s),l(r,0:1,s),u(r,0:1,s),vhit(nvmax)
      real t
      real v(nvmax,d)
      external ehg182
      save execnt
      data execnt /0/
      execnt=execnt+1
      h=nv
      do 3 i=1,r
         do 4 j=1,s
            h=h+1
            do 5 i3=1,d
               v(h,i3)=v(f(i,0,j),i3)
    5       continue
            v(h,k)=t
c           check for redundant vertex
            match=.false.
            m=1
c           top of while loop
    6       if(.not.match)then
               i1=(m.le.nv)
            else
               i1=.false.
            end if
            if(.not.(i1))goto 7
               match=(v(m,1).eq.v(h,1))
               mm=2
c              top of while loop
    8          if(match)then
                  i2=(mm.le.d)
               else
                  i2=.false.
               end if
               if(.not.(i2))goto 9
                  match=(v(m,mm).eq.v(h,mm))
                  mm=mm+1
                  goto 8
c              bottom of while loop
    9          m=m+1
               goto 6
c           bottom of while loop
    7       m=m-1
            if(match)then
               h=h-1
            else
               m=h
               if(vhit(1).ge.0)then
                  vhit(m)=p
               end if
            end if
            l(i,0,j)=f(i,0,j)
            l(i,1,j)=m
            u(i,0,j)=m
            u(i,1,j)=f(i,1,j)
    4    continue
    3 continue
      nv=h
      if(.not.(nv.le.nvmax))then
         call ehg182(180)
      end if
      return
      end
