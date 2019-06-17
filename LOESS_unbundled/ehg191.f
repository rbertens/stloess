      subroutine ehg191(m,z,l,d,n,nf,nv,ncmax,vc,a,xi,lo,hi,c,v,nvmax,vv
     $al2,lf,lq)
      integer lq1,d,execnt,i,i1,i2,j,m,n,nc,ncmax,nf,nv,nvmax,p,vc
      integer lq(nvmax,nf),a(ncmax),c(vc,ncmax),hi(ncmax),lo(ncmax)
      real l(m,n),lf(0:d,nvmax,nf),v(nvmax,d),vval2(0:d,nvmax),xi(ncmax)
     $,z(m,d),zi(8)
      real ehg128
      external ehg128
      save execnt
      data execnt /0/
      execnt=execnt+1
      do 3 j=1,n
         do 4 i2=1,nv
            do 5 i1=0,d
               vval2(i1,i2)=0
    5       continue
    4    continue
         do 6 i=1,nv
c           linear search for i in Lq
            lq1=lq(i,1)
            lq(i,1)=j
            p=nf
c           top of while loop
    7       if(.not.(lq(i,p).ne.j))goto 8
               p=p-1
               goto 7
c           bottom of while loop
    8       lq(i,1)=lq1
            if(lq(i,p).eq.j)then
               do 9 i1=0,d
                  vval2(i1,i)=lf(i1,i,p)
    9          continue
            end if
    6    continue
         do 10 i=1,m
            do 11 i1=1,d
               zi(i1)=z(i,i1)
   11       continue
            l(i,j)=ehg128(zi,d,ncmax,vc,a,xi,lo,hi,c,v,nvmax,vval2)
   10    continue
    3 continue
      return
      end
