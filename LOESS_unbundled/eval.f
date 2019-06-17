      real function ehg128(z,d,ncmax,vc,a,xi,lo,hi,c,v,nvmax,vval)
      logical i10,i2,i3,i4,i5,i6,i7,i8,i9
      integer d,execnt,i,i1,i11,i12,ig,ii,j,lg,ll,m,nc,ncmax,nt,nv,nvmax
     $,ur,vc
      integer a(ncmax),c(vc,ncmax),hi(ncmax),lo(ncmax),t(20)
      real ge,gn,gs,gw,gpe,gpn,gps,gpw,h,phi0,phi1,psi0,psi1,s,sew,sns,v
     $0,v1,xibar
      real g(0:8,256),g0(0:8),g1(0:8),v(nvmax,d),vval(0:d,nvmax),xi(ncma
     $x),z(d)
      external ehg182,ehg184
      save execnt
      data execnt /0/
      execnt=execnt+1
c     locate enclosing cell
      nt=1
      t(nt)=1
      j=1
c     top of while loop
    3 if(.not.(a(j).ne.0))goto 4
         nt=nt+1
c     bug fix 2006-07-18 (thanks, btyner@gmail.com)
         if(z(a(j)).le.xi(j))then
            i1=lo(j)
         else
            i1=hi(j)
         end if
         t(nt)=i1
         if(.not.(nt.lt.20))then
            call ehg182(181)
         end if
         j=t(nt)
         goto 3
c     bottom of while loop
    4 continue
c     tensor
      do 5 i12=1,vc
         do 6 i11=0,d
            g(i11,i12)=vval(i11,c(i12,j))
    6    continue
    5 continue
      lg=vc
      ll=c(1,j)
      ur=c(vc,j)
      do 7 i=d,1,-1
         h=(z(i)-v(ll,i))/(v(ur,i)-v(ll,i))
         if(h.lt.-.001)then
            call ehg184('eval ',z,d,1)
            call ehg184('lowerlimit ',v(ll,1),d,nvmax)
         else
            if(1.001.lt.h)then
               call ehg184('eval ',z,d,1)
               call ehg184('upperlimit ',v(ur,1),d,nvmax)
            end if
         end if
         if(-.001.le.h)then
            i2=(h.le.1.001)
         else
            i2=.false.
         end if
         if(.not.i2)then
            call ehg182(122)
         end if
         lg=float(lg)/2.
         do 8 ig=1,lg
c           Hermite basis
            phi0=(1-h)**2*(1+2*h)
            phi1=h**2*(3-2*h)
            psi0=h*(1-h)**2
            psi1=h**2*(h-1)
            g(0,ig)=phi0*g(0,ig)+phi1*g(0,ig+lg)+(psi0*g(i,ig)+psi1*g(i,
     $ig+lg))*(v(ur,i)-v(ll,i))
            do 9 ii=1,i-1
               g(ii,ig)=phi0*g(ii,ig)+phi1*g(ii,ig+lg)
    9       continue
    8    continue
    7 continue
      s=g(0,1)
c     blending
      if(d.eq.2)then
c        ----- North -----
         v0=v(ll,1)
         v1=v(ur,1)
         do 10 i11=0,d
            g0(i11)=vval(i11,c(3,j))
   10    continue
         do 11 i11=0,d
            g1(i11)=vval(i11,c(4,j))
   11    continue
         xibar=v(ur,2)
         m=nt-1
c        top of while loop
   12    if(m.eq.0)then
            i4=.true.
         else
            if(a(t(m)).eq.2)then
               i3=(xi(t(m)).eq.xibar)
            else
               i3=.false.
            end if
            i4=i3
         end if
         if(.not.(.not.i4))goto 13
            m=m-1
c           voidp junk
            goto 12
c        bottom of while loop
   13    if(m.ge.1)then
            m=hi(t(m))
c           top of while loop
   14       if(.not.(a(m).ne.0))goto 15
               if(z(a(m)).le.xi(m))then
                  m=lo(m)
               else
                  m=hi(m)
               end if
               goto 14
c           bottom of while loop
   15       if(v0.lt.v(c(1,m),1))then
               v0=v(c(1,m),1)
               do 16 i11=0,d
                  g0(i11)=vval(i11,c(1,m))
   16          continue
            end if
            if(v(c(2,m),1).lt.v1)then
               v1=v(c(2,m),1)
               do 17 i11=0,d
                  g1(i11)=vval(i11,c(2,m))
   17          continue
            end if
         end if
         h=(z(1)-v0)/(v1-v0)
c        Hermite basis
         phi0=(1-h)**2*(1+2*h)
         phi1=h**2*(3-2*h)
         psi0=h*(1-h)**2
         psi1=h**2*(h-1)
         gn=phi0*g0(0)+phi1*g1(0)+(psi0*g0(1)+psi1*g1(1))*(v1-v0)
         gpn=phi0*g0(2)+phi1*g1(2)
c        ----- South -----
         v0=v(ll,1)
         v1=v(ur,1)
         do 18 i11=0,d
            g0(i11)=vval(i11,c(1,j))
   18    continue
         do 19 i11=0,d
            g1(i11)=vval(i11,c(2,j))
   19    continue
         xibar=v(ll,2)
         m=nt-1
c        top of while loop
   20    if(m.eq.0)then
            i6=.true.
         else
            if(a(t(m)).eq.2)then
               i5=(xi(t(m)).eq.xibar)
            else
               i5=.false.
            end if
            i6=i5
         end if
         if(.not.(.not.i6))goto 21
            m=m-1
c           voidp junk
            goto 20
c        bottom of while loop
   21    if(m.ge.1)then
            m=lo(t(m))
c           top of while loop
   22       if(.not.(a(m).ne.0))goto 23
               if(z(a(m)).le.xi(m))then
                  m=lo(m)
               else
                  m=hi(m)
               end if
               goto 22
c           bottom of while loop
   23       if(v0.lt.v(c(3,m),1))then
               v0=v(c(3,m),1)
               do 24 i11=0,d
                  g0(i11)=vval(i11,c(3,m))
   24          continue
            end if
            if(v(c(4,m),1).lt.v1)then
               v1=v(c(4,m),1)
               do 25 i11=0,d
                  g1(i11)=vval(i11,c(4,m))
   25          continue
            end if
         end if
         h=(z(1)-v0)/(v1-v0)
c        Hermite basis
         phi0=(1-h)**2*(1+2*h)
         phi1=h**2*(3-2*h)
         psi0=h*(1-h)**2
         psi1=h**2*(h-1)
         gs=phi0*g0(0)+phi1*g1(0)+(psi0*g0(1)+psi1*g1(1))*(v1-v0)
         gps=phi0*g0(2)+phi1*g1(2)
c        ----- East -----
         v0=v(ll,2)
         v1=v(ur,2)
         do 26 i11=0,d
            g0(i11)=vval(i11,c(2,j))
   26    continue
         do 27 i11=0,d
            g1(i11)=vval(i11,c(4,j))
   27    continue
         xibar=v(ur,1)
         m=nt-1
c        top of while loop
   28    if(m.eq.0)then
            i8=.true.
         else
            if(a(t(m)).eq.1)then
               i7=(xi(t(m)).eq.xibar)
            else
               i7=.false.
            end if
            i8=i7
         end if
         if(.not.(.not.i8))goto 29
            m=m-1
c           voidp junk
            goto 28
c        bottom of while loop
   29    if(m.ge.1)then
            m=hi(t(m))
c           top of while loop
   30       if(.not.(a(m).ne.0))goto 31
               if(z(a(m)).le.xi(m))then
                  m=lo(m)
               else
                  m=hi(m)
               end if
               goto 30
c           bottom of while loop
   31       if(v0.lt.v(c(1,m),2))then
               v0=v(c(1,m),2)
               do 32 i11=0,d
                  g0(i11)=vval(i11,c(1,m))
   32          continue
            end if
            if(v(c(3,m),2).lt.v1)then
               v1=v(c(3,m),2)
               do 33 i11=0,d
                  g1(i11)=vval(i11,c(3,m))
   33          continue
            end if
         end if
         h=(z(2)-v0)/(v1-v0)
c        Hermite basis
         phi0=(1-h)**2*(1+2*h)
         phi1=h**2*(3-2*h)
         psi0=h*(1-h)**2
         psi1=h**2*(h-1)
         ge=phi0*g0(0)+phi1*g1(0)+(psi0*g0(2)+psi1*g1(2))*(v1-v0)
         gpe=phi0*g0(1)+phi1*g1(1)
c        ----- West -----
         v0=v(ll,2)
         v1=v(ur,2)
         do 34 i11=0,d
            g0(i11)=vval(i11,c(1,j))
   34    continue
         do 35 i11=0,d
            g1(i11)=vval(i11,c(3,j))
   35    continue
         xibar=v(ll,1)
         m=nt-1
c        top of while loop
   36    if(m.eq.0)then
            i10=.true.
         else
            if(a(t(m)).eq.1)then
               i9=(xi(t(m)).eq.xibar)
            else
               i9=.false.
            end if
            i10=i9
         end if
         if(.not.(.not.i10))goto 37
            m=m-1
c           voidp junk
            goto 36
c        bottom of while loop
   37    if(m.ge.1)then
            m=lo(t(m))
c           top of while loop
   38       if(.not.(a(m).ne.0))goto 39
               if(z(a(m)).le.xi(m))then
                  m=lo(m)
               else
                  m=hi(m)
               end if
               goto 38
c           bottom of while loop
   39       if(v0.lt.v(c(2,m),2))then
               v0=v(c(2,m),2)
               do 40 i11=0,d
                  g0(i11)=vval(i11,c(2,m))
   40          continue
            end if
            if(v(c(4,m),2).lt.v1)then
               v1=v(c(4,m),2)
               do 41 i11=0,d
                  g1(i11)=vval(i11,c(4,m))
   41          continue
            end if
         end if
         h=(z(2)-v0)/(v1-v0)
c        Hermite basis
         phi0=(1-h)**2*(1+2*h)
         phi1=h**2*(3-2*h)
         psi0=h*(1-h)**2
         psi1=h**2*(h-1)
         gw=phi0*g0(0)+phi1*g1(0)+(psi0*g0(2)+psi1*g1(2))*(v1-v0)
         gpw=phi0*g0(1)+phi1*g1(1)
c        NS
         h=(z(2)-v(ll,2))/(v(ur,2)-v(ll,2))
c        Hermite basis
         phi0=(1-h)**2*(1+2*h)
         phi1=h**2*(3-2*h)
         psi0=h*(1-h)**2
         psi1=h**2*(h-1)
         sns=phi0*gs+phi1*gn+(psi0*gps+psi1*gpn)*(v(ur,2)-v(ll,2))
c        EW
         h=(z(1)-v(ll,1))/(v(ur,1)-v(ll,1))
c        Hermite basis
         phi0=(1-h)**2*(1+2*h)
         phi1=h**2*(3-2*h)
         psi0=h*(1-h)**2
         psi1=h**2*(h-1)
         sew=phi0*gw+phi1*ge+(psi0*gpw+psi1*gpe)*(v(ur,1)-v(ll,1))
         s=(sns+sew)-s
      end if
      ehg128=s
      return
      end
