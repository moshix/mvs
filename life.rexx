 /* CONWAY'S GAME OF LIFE FOR REXX  */
 /* FOR BREXX ON MVS 3.8 and VM     */
 /* COPYRIGHT 2024 BY MOSHIX        */
 version="0.2"
 rows = 20
 cols = 30
 generations = 5

 say 'Conway Game of life for BREXX'
 say '============================='
 say ' '
 say 'Nr. Rows for this run: 'rows
 say 'Nr. Cols for this run: 'cols
 say ' '
 /* initialize the board with random data */


 do row = 1 to rows
    do col = 1 to cols
       world.row.col = random(0,1)
    end
 end

 /* display the initial random world */

 say 'Initial, random world'
 say '====================='
 say ' '
 call DisplayWorld


 /* run throu generations */
 do gen = 1 to generations
    say ' '
    say 'Genneration nr: 'gen
    say '------------------'
    say ' '
    call NextGeneration
    say ' '
    call DisplayWorld
 end

 exit /* end of the program */


 NextGeneration:
 /* implement the rules of Conways Game of life */
 do row = 1 to rows
   do col = 1 to cols
      neighbors = CountNeighbors(row, col)
      if world.row.col = 1 then do
         if neighbors < 2 | neighbors > 3 then
            temp.row.col = 0
         else
            temp.row.col = 1
         end
      else do
         if neighbors = 3 then
              temp.row.col = 1
         else
              temp.row.col = 0
      end
   end
 end

 /* copy temp map to display map */
 do row = 1 to rows
    do col = 1 to cols
       world.row.col = temp.row.col
    end
 end
return


CountNeighbors:
/* count neighbors per cell */
parse arg row,col
neighbors = 0
do dr = -1 to 1
   do dc = -1 to 1
      if dr = 0 & dc = 0 then iterate
      nr = row + dr
      nc = col + dc
      if nr > 0 & nr <= rows & nc > 0 & nc <= cols then
         neighbors = neighbors + world.nr.nc
      end
end
return neighbors


 DisplayWorld:
 /* show the map on the screen */
 do row = 1 to rows
     line = ''
     do col = 1 to cols
        if world.row.col = 1 then
           line = line || '#'
        else
           line = line || ' '
     end
     say line
 end
 return

