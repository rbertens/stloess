      subroutine ehg124(ll,uu,d,n,nv,nc,ncmax,vc,x,pi,a,xi,lo,hi,c,v,vhi
     $t,nvmax,fc,fd,dd)
      logical i1,i2,i3,leaf
      integer d,dd,execnt,fc,i4,inorm2,k,l,ll,m,n,nc,ncmax,nv,nvmax,p,u,
     $uu,vc,lower,upper,check,offset
      integer a(ncmax),c(vc,ncmax),hi(ncmax),lo(ncmax),pi(n),vhit(nvmax)
      real diam,fd
      real diag(8),sigma(8),v(nvmax,d),x(n,d),xi(ncmax)
      external ehg125,ehg106,ehg129
      integer isamax
      external isamax
      save execnt
      data execnt /0/
      execnt=execnt+1
      p=1
      l=ll
      u=uu
      lo(p)=l
      hi(p)=u
c     top of while loop
    3 if(.not.(p.le.nc))goto 4
         do 5 i4=1,dd
            diag(i4)=v(c(vc,p),i4)-v(c(1,p),i4)
    5    continue
         diam=0
         do 6 inorm2=1,dd
            diam=diam+diag(inorm2)**2
    6    continue
         diam=sqrt(diam)
         if((u-l)+1.le.fc)then
            i1=.true.
         else
            i1=(diam.le.fd)
         end if
         if(i1)then
            leaf=.true.
         else
            if(ncmax.lt.nc+2)then
               i2=.true.
            else
               i2=(nvmax.lt.nv+float(vc)/2.)
            end if
            leaf=i2
         end if
         if(.not.leaf)then
            call ehg129(l,u,dd,x,pi,n,sigma)
            k=isamax(dd,sigma,1)
            m=float(l+u)/2.
            call ehg106(l,u,m,1,x(1,k),pi,n)

c bug fix from btyner@gmail.com 2006-07-20
      offset = 0
    7 if(((m+offset).ge.u).or.((m+offset).lt.l))then
         goto 8
      else
        if(offset.lt.0)then
          lower = l
          check = m + offset
          upper = check
        else
          lower = m + offset + 1
          check = lower
          upper = u
        end if
        call ehg106(lower,upper,check,1,x(1,k),pi,n)
        if(x(pi(m + offset),k).eq.x(pi(m+offset+1),k))then
          offset = (-offset)
          if(offset.ge.0)then
            offset = offset + 1
          end if
      goto 7
        else
          m = m + offset
          goto 8
        end if
      end if

    8       if(v(c(1,p),k).eq.x(pi(m),k))then
               leaf=.true.
            else
               leaf=(v(c(vc,p),k).eq.x(pi(m),k))
            end if
         end if
         if(leaf)then
            a(p)=0
         else
            a(p)=k
            xi(p)=x(pi(m),k)
c           left son
            nc=nc+1
            lo(p)=nc
            lo(nc)=l
            hi(nc)=m
c           right son
            nc=nc+1
            hi(p)=nc
            lo(nc)=m+1
            hi(nc)=u
            call ehg125(p,nv,v,vhit,nvmax,d,k,xi(p),2**(k-1),2**(d-k),c(
     $1,p),c(1,lo(p)),c(1,hi(p)))
         end if
         p=p+1
         l=lo(p)
         u=hi(p)
         goto 3
c     bottom of while loop
    4 return
      end
