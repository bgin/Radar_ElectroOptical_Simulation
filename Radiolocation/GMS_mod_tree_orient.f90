

module mod_tree_orient

     use mod_kinds,  only : int4
     implicit none
     public


     ! Leaf distribution variables
     save

     integer(kind=int4) :: leaf_orient
     integer(kind=int4) :: branch_orient1
     integer(kind=int4) :: branch_orient2
     integer(kind=int4) :: branch_orient3
     integer(kind=int4) :: branch_orient4
     integer(kind=int4) :: branch_orient5
     integer(kind=int4) :: branch_orient6
     integer(kind=int4) :: trunk_orient

     
end module mod_tree_orient
