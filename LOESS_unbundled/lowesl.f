      subroutine lowesl(iv,liv,lv,wv,m,z,l)
      integer execnt,m,n
      integer iv(*)
      real l(m,*),wv(*),z(m,1)
      external ehg182,ehg191
      save execnt
      data execnt /0/
      execnt=execnt+1
      if(.not.(iv(28).ne.172))then
         call ehg182(172)
      end if
      if(.not.(iv(28).eq.173))then
         call ehg182(173)
      end if
      if(.not.(iv(26).ne.iv(34)))then
         call ehg182(175)
      end if
      call ehg191(m,z,l,iv(2),iv(3),iv(19),iv(6),iv(17),iv(4),iv(iv(7)),
     $wv(iv(12)),iv(iv(10)),iv(iv(9)),iv(iv(8)),wv(iv(11)),iv(14),wv(iv(
     $24)),wv(iv(34)),iv(iv(25)))
      return
      end
