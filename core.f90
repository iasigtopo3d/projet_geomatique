!COPYRIGHT
!WORLD COMPANY IASIG 2013
!GSE - OTH - PFO



!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!MODULE TABLES_DE_GRAPHE
! ce module contient seulement la description des tables exprimant la topologie d'un graphe.
!SIF, SXY, PAXY
!   -NUMRECORD, retourne le nombre d'enregistrements des differents fichiers
!   -READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements
!   -SPLIT, ce charge de separer une ligne du fichier en element indice.
!***********************************************************************************************************!

module tables_de_graphe

implicit none

!***********************************************************************************************************!
!Declaration
   integer                               :: NA, NS, ND,NAXY
   integer, dimension (:,:), allocatable :: SIF, GD
   integer, dimension (:,:), allocatable :: SXY, AXY, PAXY


end module tables_de_graphe
!***********************************************************************************************************!



!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!MODULE LIRE_DONNEES
!Ce module contient les fonctions necessaires pour recuperer les donnees stockees dans les tables:
!SIF, SXY, PAXY
!   -NUMRECORD, retourne le nombre d'enregistrements des differents fichiers
!   -READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements
!   -SPLIT, ce charge de separer une ligne du fichier en element indice.
!***********************************************************************************************************!

module lire_donnees

implicit none
contains

    !***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    subroutine READFILE(pathFile, tab, n)
    !READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements
    !   -pathFile :: chemins d'acces au fichier
    !   -tab :: tableau d'element retourne par le fichier
    !   -n :: nombre d'elements contenu dans le tableau
    !***********************************************************************************************************!
    !Specification

        character(200), intent(in)      ::pathFile
        integer,  intent(out),allocatable, dimension(:,:)      ::tab
        integer, intent(in)             ::n

    !***********************************************************************************************************!
    !Declaration
        logical                         ::exist
        integer                         ::io,  i, j, nelmt
        character(20)                   ::str
        character(1)                    ::sep
        integer, dimension(3)           ::elmt

    !***********************************************************************************************************!
    !Body
        allocate(tab(n,3))
        !Verification de l'existence du fichier
        inquire(file=pathFile,exist= exist)
        if (.NOT. exist) then
            print *, "Erreur! Fichier non existant"
            return
        endif

        !Ouverture du fichier
        open(10, file=pathFile)
        !Lecture du fichier sur n lignes (on les a recupere avec NUMRECORD
        do i = 1, n
            read(10,1000,iostat=io)str
            !Test de fin de fichier
            if(io < 0) exit
            sep = ";"
            !Recuperation des elements contenus dans la chaene
            call SPLIT(str, sep, elmt, nelmt)

            do j = 1, nelmt
                tab(i,j) = elmt(j)
                end do
        enddo
        !Fermeture du fichier
        close(10)

    deallocate (tab)
    1000 FORMAT(a10)
    end subroutine READFILE

    !***********************************************************************************************************!
    !***********************************************************************************************************!
    !SUBROUTINE
    subroutine NUMRECORD(pathFile, n)
    !READFILE, lit les fichiers et retourne un tableau de donnees, ainsi que le nombre d'enregistrements

    !***********************************************************************************************************!
    !Specification

        character(200), intent(in)      ::pathFile
        integer, intent(out)            ::n

    !***********************************************************************************************************!
    !Declaration
        logical                         ::exist
        integer                         ::io

        n = 0
    !***********************************************************************************************************!
    !Body
        inquire(file=pathFile,exist= exist)
        if (.NOT. exist) then
            print *, "Erreur! Fichier non existant"
            return
        endif

        !Ouverture du fichier
        open(10, file=pathFile)
        !Lecture du fichier
        do
            read(10,*,iostat=io)
            !Test si fin du fichier
            if(io < 0) exit
            n = n + 1

        enddo
        !Fermeture du fichier
        close(10)

    end subroutine NUMRECORD


    !***********************************************************************************************************!
    !-----------------------------
    !SUBROUTINE
    subroutine SPLIT(str, sep, T, n)
    !SPLIT, ce charge de separer une ligne du fichier en element indice.
    !   -str :: la chaene de caractere e traiter
    !   -sep :: le separateur d'elements
    !   - T :: le tableau d'element qui sera retourne. La taille peut-etre variable (XY ou XYZ, etc.)
    !   - n :: le nombred'element dans le tabeau.

    !-----------------------------
    !Specification
        character(20), intent(in)       ::str
        character(1), intent(in)       ::sep
        integer, dimension(:), intent(out) ::T
        integer, intent(out)            ::n

    !-----------------------------
    !Declaration
        integer             :: pos1, pos2, val

    !-----------------------------
    !Body
        pos1 = 1
        n = 0

       do
            pos2 = index( str(pos1:len_trim(str)),sep)
            if (pos2 == 0) then
                n = n + 1
                !Convertir la chaine de caractere en entier
                read(str(pos1:), *) val
                T(n) = val
                exit
            end if
            n = n + 1
            !Convertir la chaine de caractere en entier
            read(str(pos1:pos1 + pos2 - 2), *) val
            T(n) = val
            pos1 = pos1 + pos2
        end do

    end subroutine SPLIT

end module lire_donnees



!***********************************************************************************************************!
!***********************************************************************************************************!
!***********************************************************************************************************!
!MODULE TOPOLOGIE
! ce module contient les procedures de base operant sur la topologie d'un graphe :
!   - ESS , entrants et sortants d'un sommet
!   - PSS , predecesseurs et successeursq d'un sommet
!   - PSA , predecesseurs et successeurs d'un arc
!   - DIRETRO , arcs directs et retrogrades d'un domaine
!   - ADJ , arcs adjacents a un domaine
!   - PERIPH, sommets peripheriques d'un domaine
!   - ENTOUR, domaines entourant un sommet
!       ces deux derniers modules utilisent la procedure
!       PERI_ENTOUR , donnant :
!             -soit les sommets peripheriques d'un domaine
!             -soit les domaines entourant un sommet
!   - OPP , domaines opposes par un sommet a un domaine
!***********************************************************************************************************!
module topologie

!***********************************************************************************************************!
!Declaration
use tables_de_graphe

implicit none
           private PERI_ENTOUR
                                                              !
   contains                                                   !
                                                              !
   !===========================================================
   subroutine ESS (s,  TA, nta, k )                           !
   !----------------------------------------------------------!
   !    donne pour un sommet s, dans un vecteur TA,           !
   !    de cardinalité nta, la liste des arcs :               !
   !       -sortant de s, pour k=1                            !
   !       -entrant dans s, pour k=2                          !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: s             !
      integer, intent (out),dimension (1:10) :: TA            !
      integer, intent (out)                  :: nta           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i                                           !
   !----------------------------------------------------------!
   !                           corps                          !
     nta = 0                                                  !
      do i = 1, NA                                            !
         if ( SIF(i,k) /= s ) then                            !
             cycle                                            !
         else                                                 !
         endif                                                !
         nta = nta + 1 ; TA (nta)= i                          !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine !--ESS--                                    !
   !===========================================================

   !===========================================================
   subroutine PSS (s, TS, nts, k )                            !
   !----------------------------------------------------------!
   !   donne pour le sommet s, dans un vecteur TS,            !
   !   de cardinalité nts, la liste :                         !
   !      -des sommets successeurs, pour k=1                  !
   !      -des sommets prédécesseurs, pour k=2                !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: s                 !
      integer, intent (out),dimension (1:NA) :: TS            !
      integer, intent (out)                  :: nts           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i, j                                        !
   !----------------------------------------------------------!
   !                           corps                          !
    nts = 0                                                   !
    bou: do i = 1, NA ;                                       !
           if ( SIF(i,k) /= s ) then                          !
              cycle                                           !
           else                                               !
           endif                                              !
           if ( nts /= 0 ) then                               !
              do j = 1 , nts                                  !
                 if ( TS(j) == SIF(i,3-k)) then               !
                    cycle bou                                 !
                 else                                         !
                 endif                                        !
              enddo !--j--                                    !
           else                                               !
           endif                                              !
                                                              !
           nts = nts + 1 ; TS(nts)=SIF(i, 3-k)                !
         enddo bou !--i--                                     !
   !----------------------------------------------------------!
   end subroutine !--PSS--                                    !
   !===========================================================

   !===========================================================
   subroutine PSA (a, TA, nta, k )                            !
   !----------------------------------------------------------!
   !   donne pour l'arc a, dans une vecteur TA,               !
   !   de cardinalité nta, la liste des arcs :                !
   !       -les successeurs de a, lorsque k=1                 !
   !       -les predecesseurs de a, lorsque k=2               !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: a             !
      integer, intent (out),dimension (1:10) :: TA            !
      integer, intent (out)                  :: nta           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i                                           !
   !----------------------------------------------------------!
   !                           corps                          !
     nta = 0                                                  !
      do i = 1, NA                                            !
         if ( SIF(i,3-k) /= SIF(a,k) ) then                   !
             cycle                                            !
         else                                                 !
         endif                                                !
         nta = nta + 1 ; TA (nta)= i                          !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine !--PSA--                                    !
   !===========================================================

   !===========================================================
   subroutine DIRETRO ( d , TA, nta, k )                      !
   !----------------------------------------------------------!
   !   donne pour le domaine d , dans le vecteur TA,          !
   !   de cardinalité nta,                                    !
   !      -les arcs directs, lorsque k=1                      !
   !      -les arcs rétrogrades, lorsque k=2                  !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: d             !
      integer, intent (out),dimension (1:10) :: TA            !
      integer, intent (out)                  :: nta           !
      integer, intent (in)                   :: k             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i                                           !
   !----------------------------------------------------------!
   !                           corps                          !
     nta = 0                                                  !
      do i = 1, NA                                            !
         if ( GD(i,k) /= d ) then                             !
             cycle                                            !
         else                                                 !
         endif                                                !
         nta = nta + 1 ; TA (nta)= i                          !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine !--DIRETRO--                                !
   !===========================================================

   !===========================================================
   subroutine ADJ (d, TD, ntd )                               !
   !----------------------------------------------------------!
   !   donne pour le domaine d , dans le vecteur TD,          !
   !   de cardinalité ntd,les domaines adjacents à d.         !                           !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: d             !
      integer, intent (out),dimension (1:10) :: TD            !
      integer, intent (out)                  :: ntd           !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer ::  i, j, k                                     !
   !----------------------------------------------------------!
   !                           corps                          !
     ntd = 0                                                  !
     do i = 1, NA                                             !
       bou: do j = 1, 2                                       !
               if ( GD(i,j) /= d ) then                       !
                  cycle                                       !
               else                                           !
               endif                                          !
               if ( ntd /= 0 ) then                           !
                  do k = 1, ntd                               !
                     if (TD(k) == GD(i, 3-j) ) then           !
                        cycle bou    ! next 2                 !
                     else                                     !
                     endif                                    !
                  enddo !--k--                                !
               else                                           !
               endif                                          !
               ntd = ntd + 1 ; TD (ntd)= GD(i, 3-j)           !
            enddo bou  !--j--                                 !
       enddo !--i--                                           !
   !----------------------------------------------------------!
   end subroutine ADJ                                         !
   !===========================================================

   !===========================================================
   subroutine PERIPH (d, T, nt )                              !
   !----------------------------------------------------------!
   ! donne dans le vecteur T, de cardinalite nt, la liste des !
   ! sommets peripheriques du domaine d                       !
   !----------------------------------------------------------!
   !                       specifications                     !
     integer, intent (in)                   :: d              !
     integer, intent (out),dimension(1:10)  :: T              !
     integer, intent (out)                  :: nt             !
   !----------------------------------------------------------!
   !                        declarations                      !
   !----------------------------------------------------------!
   !                          externes                        !
!     interface                                               !
       !.................................................     !
!      subroutine PERI_ENTOUR (c, A, B, T, nt )               !
!         integer, intent (in)                   :: c         !
!         integer, intent (in), dimension(:,:)   :: A,B       !
!         integer, intent (out),dimension(1:10)  :: T         !
!         integer, intent (out)                  :: nt        !
!      end subroutine ! -- PERI_ENTOUR --                     !
       !..................................................    !
!    end interface                                            !
   !----------------------------------------------------------!
   !                           corps                          !
        call PERI_ENTOUR (d, GD, SIF, T, nt )                 !
   !----------------------------------------------------------!
   end subroutine ! -- PERIPH --                              !
   !===========================================================

   !===========================================================
   subroutine ENTOUR (s, T, nt )                              !
   !----------------------------------------------------------!
   ! donne dans le vecteur T, de cardinalite nt, la liste des !
   ! domaines entourant le sommet s                           !
   !----------------------------------------------------------!
   !                       specifications                     !
     integer, intent (in)                   :: s              !
     integer, intent (out),dimension(1:10)  :: T              !
     integer, intent (out)                  :: nt             !
   !----------------------------------------------------------!
   !                        declarations                      !
   !----------------------------------------------------------!
   !                          externes                        !
!     interface                                               !
       !.................................................     !
!      subroutine PERI_ENTOUR (c, A, B, T, nt )               !
!         integer, intent (in)                   :: c         !
!         integer, intent (in), dimension(:,:)   :: A,B       !
!         integer, intent (out),dimension(1:10)  :: T         !
!          integer, intent (out)                 :: nt        !
!      end subroutine ! -- PERI_ENTOUR --                     !
       !..................................................    !
!    end interface                                            !
   !----------------------------------------------------------!
   !                           corps                          !
        call PERI_ENTOUR (s, SIF, GD, T, nt )                 !
   !----------------------------------------------------------!
   end subroutine !-- ENTOUR --                               !
   !===========================================================

   !===========================================================
   subroutine PERI_ENTOUR (c, A, B, T, nt )                   !
   !----------------------------------------------------------!
   !  donne pour le composant c, un vecteur de cardinalite nt.!
   !    - c : domaine, T contient les sommets peripheriques   !
   !    - c : sommet, T contient les domaines entourants.     !
   !----------------------------------------------------------!
   !                       specifications                     !
     integer, intent (in)                   :: c              !
     integer, intent (in), dimension(:,:)   :: A,B            !
     integer, intent (out),dimension(1:10)  :: T              !
     integer, intent (out)                  :: nt             !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer    :: i, j, k, h                                !
   !----------------------------------------------------------!
      nt = 0                                                  !
      do i = 1, NA                                            !
         do j = 1, 2                                          !
            if ( A(i,j) /= c ) then                           !
               cycle                                          !
            else                                              !
            endif                                             !
            bou : do k = 1, 2                                 !
                     if ( nt /= 0 ) then                      !
                        do h = 1, nt                          !
                           if ( T(h) == B(i,k) ) then         !
                               cycle bou       ! next 2       !
                           else                               !
                           endif                              !
                        enddo  !--h-- ;                       !
                     else                                     !
                     endif                                    !
                     nt = nt + 1 ; T(nt) = B(i,k)             !
                   enddo bou  !--k--                          !
          enddo !--j--                                        !
      enddo !--i--                                            !
   !----------------------------------------------------------!
   end subroutine ! -- PERI_ENTOUR --                         !
   !===========================================================

   !===========================================================
   subroutine OPP (d, T, nt )                                 !
   !----------------------------------------------------------!
   !   donne pour le domaine d , dans le vecteur T ,          !
   !   de cardinalité nt ,les domaines opposes à d.           !
   !----------------------------------------------------------!
   !                       specifications                     !
      integer, intent (in)                   :: d             !
      integer, intent (out),dimension (1:10) :: T             !
      integer, intent (out)                  :: nt            !
   !----------------------------------------------------------!
   !                        declarations                      !
      integer                    :: nl,i,j,k,h,a,s,dd         !
      integer, dimension (1:100) ::  L                        !
   !----------------------------------------------------------!
   !                           corps                          !
     nt  = 0                                                  !
     do i = 1, NA                                             !
       do j = 1, 2                                            !
         if ( GD(i,j) /= d ) then                             !
            cycle                                             !
         else                                                 !
         endif                                                !
         nt = nt + 1 ; T(nt) = i                              !
       enddo !--j--                                           !
     enddo !--i--                                             !
                                                              !
   ! on obtient ici tous les arcs, directs ou retrogrades,    !
   ! entourant le domaine d, et ceci sans duplicata           !
     nl = 0                                                   !
     do i = 1, nt                                             !
        a = T (i)                                             !
        bou_1 : do j = 1, 2 ;                                 !
                   if ( nl /= 0 ) then                        !
                      do  k = 1, nl                           !
                         if ( L(k) == SIF(a,j) ) then         !
                             cycle bou_1                      !
                         else                                 !
                         endif                                !
                      enddo !--k--                            !
                   else                                       !
                   endif                                      !
                   nl = nl + 1 ; L (nl) =  SIF (a, j)         !
                 enddo bou_1  !--j--                          !
      enddo !--i--                                            !
                                                              !
    ! on a  les sommets peripheriques de d sans duplicata     !
                                                              !
      nt = 0 ;                                                !
      do i = 1 , nl                                           !
         s = L(i)                                             !
         do j = 1, NA                                         !
           bou_2 : do k = 1, 2                                !
                     if ( SIF(j,k) /= s ) then                !
                         cycle                                !
                     else                                     !
                     endif                                    !
                     if ( nt /= 0 ) then                      !
                        do h = 1, nt ;                        !
                           if ( T(h) == j ) then              !
                              cycle bou_2                     !
                           else                               !
                           endif                              !
                        enddo !--h--                          !
                     else                                     !
                     endif                                    !
                     nt = nt + 1 ; T(nt) = j                  !
                    enddo bou_2   !--k--                      !
          enddo !--j--                                        !
      enddo !--i--                                            !
                                                              !
    ! on a tous les arcs entrants et sortants sans duplicata  !
                                                              !
      nl = 0                                                  !
      do i = 1, nt                                            !
         a = T(i)                                             !
         bou_3 : do j = 1,2                                   !
                    if ( nl /= 0 ) then                       !
                       do k = 1, nl                           !
                          if ( L(k) == GD(a,j) ) then         !
                             cycle  bou_3                     !
                          else                                !
                          endif                               !
                       enddo !--k--                           !
                     else                                     !
                     endif                                    !
                     nl = nl + 1 ; L(nl) = GD (a,j)           !
                  enddo  bou_3  !--j--                        !
       enddo !--i--                                           !
                                                              !
    !  on a a tous les domaines concernant d,sans duplicata,  !
    !  c'est a dire : - les domaines adjacents a d,           !
    !                 - d lui-meme,                           !
    !                 - ses eventuels opposes.                !
                                                              !
      nt = 0                                                  !
      bou_4: do i = 1, nl                                     !
                dd = L(i)                                     !
                if ( dd == d ) then                           !
                    cycle     !-- c'est d lui-meme --         !
                else                                          !
                endif                                         !
                do j = 1, NA                                  !
                  if( GD(i,1) == d .AND. GD(i,2) == dd .OR.&  !
                    & GD(i,1) == dd .AND. GD(i,2) == d ) then !
                     cycle  bou_4   !-- dd est adjacent a d --!
                  else                                        !
                  endif                                       !
                enddo !--j--                                  !
                             !-- donc dd est un oppose a d -- !
                nt = nt + 1 ; T(nt) = dd                      !
             enddo  bou_4  !--i--                             !
   !----------------------------------------------------------!
   end subroutine OPP                                         !
   !===========================================================
                                                              !
!-------------------------------------------------------------!
end module topologie                                          !
!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM




!-----------------------------
!PROGRAM

program MAIN
!This program read the SIF and SXY file of a Graph
!-----------------------------
use tables_de_graphe
use lire_donnees
use topologie

!Declaration
    implicit none
    character(200)          ::path
    integer                 ::i,j
    !Variables pour PSS
    integer, allocatable, dimension (:) :: TPSS            !
    integer                :: npss
!-----------------------------

!-----------------------------
!Body
    path = "./data/SIF.txt"

    !Obtenir NA
    print *, "SUBROUTINE NUMRECORD"
    call NUMRECORD(path, NA)
    print *, "Nombre de ligne dans le fichier ", NA

    !Allocation de la table SIF
    allocate(SIF(2,NA))

    !Obtenir la table SIF
    print *, "appel de la subroutine READFILE"
    call READFILE(path, SIF, NA)
    print *, "Fin de l'appel de subroutine"

    !Affichage de la table SIF
    do i = 1, NA
        print *, "Arc:"
        do j = 1, 2
            print *, SIF(i,j)
        end do
        print *, ""
    end do

    allocate(TPSS(NA))
    call PSS (2, TPSS, npss, 1 )
    print *, "PSS"
    do i = 1, npss
        print *, TPSS(i)
    end do

end program MAIN
!-----------------------------
