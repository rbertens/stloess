      subroutine lofort(iunit,iv,liv,lv,wv)
      integer execnt,iunit
      integer iv(*)
      real wv(*)
      external ehg170
      save execnt
      data execnt /0/
      execnt=execnt+1
      call ehg170(iunit,iv(2),iv(4),iv(6),iv(14),iv(5),iv(17),iv(iv(7)),
     $iv(iv(8)),iv(iv(9)),iv(iv(10)),wv(iv(11)),wv(iv(13)),wv(iv(12)))
      return
      end
