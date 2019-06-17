      subroutine ehg170(k,d,vc,nv,nvmax,nc,ncmax,a,c,hi,lo,v,vval,xi)
      integer d,execnt,i,j,nc,ncmax,nv,nvmax,vc
      integer a(ncmax),c(vc,ncmax),hi(ncmax),lo(ncmax)
      real v(nvmax,d),vval(0:d,nvmax),xi(ncmax)
      save execnt
      data execnt /0/
      execnt=execnt+1
      write(k,*)'      real function loeval(z)'
      write(k,50)d
      write(k,*)'      integer d,vc,nv,nc'
      write(k,51)nc,vc,nc
      write(k,52)nc,nc
      write(k,53)nv,d
      write(k,54)d,nv
      write(k,55)nc
      write(k,56)
      write(k,57)d,vc,nv,nc
50    format('      real z(',i2,')')
51    format('      integer a(',i5,'), c(',i3,',',i5,')')
52    format('      integer hi(',i5,'), lo(',i5,')')
53    format('      real v(',i5,',',i2,')')
54    format('      real vval(0:',i2,',',i5,')')
55    format('      real xi(',i5,')')
56    format('      real ehg128')
57    format('      data d,vc,nv,nc /',i2,',',i3,',',i5,',',i5,'/')
      do 3 i=1,nc
         write(k,58)i,a(i)
58       format('      data a(',i5,') /',i5,'/')
         if(a(i).ne.0)then
            write(k,59)i,i,i,hi(i),lo(i),xi(i)
59          format('      data hi(',i5,'),lo(',i5,'),xi(',i5,') /',
     $          i5,',',i5,',',1pe15.6,'/')
         end if
         do 4 j=1,vc
            write(k,60)j,i,c(j,i)
60          format('      data c(',i3,',',i5,') /',i5,'/')
    4    continue
    3 continue
      do 5 i=1,nv
         write(k,61)i,vval(0,i)
61       format('      data vval(0,',i5,') /',1pe15.6,'/')
         do 6 j=1,d
            write(k,62)i,j,v(i,j)
62          format('      data v(',i5,',',i2,') /',1pe15.6,'/')
            write(k,63)j,i,vval(j,i)
63          format('      data vval(',i2,',',i5,') /',1pe15.6,'/')
    6    continue
    5 continue
      write(k,*)'      loeval=ehg128(z,d,nc,vc,a,xi,lo,hi,c,v,nv,vval)'
      write(k,*)'      end'
      return
      end
