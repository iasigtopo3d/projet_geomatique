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

implicit none ;

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
!MODULE ACCESS_DATA
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

!-----------------------------
!PROGRAM

program MAIN
!This program read the SIF and SXY file of a Graph
!-----------------------------
use tables_de_graphe
use lire_donnees
!Declaration
    implicit none
    character(200)          ::path
    integer                 ::i,j
!-----------------------------

!-----------------------------
!Body
    path = "E:\Python\graphe\SIFF.txt"

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

end program MAIN
!-----------------------------
