/* Conway's Game of Life in REXX  */

rows = 20
cols = 40
generations = 12

/* Initialize world with random data */
do row = 1 to rows
    do col = 1 to cols
        world.row.col = random(0, 1)
    end
end

/* Display the initial world */
say 'Initial World:'
call DisplayWorld

/* Run through generations */
do gen = 1 to generations
    say 'Generation:' gen
    call NextGeneration
    call DisplayWorld
end
exit

NextGeneration:
    /* Temporary world for calculations */
    do row = 1 to rows
        do col = 1 to cols
            neighbors = CountNeighbors(row, col)
            /* Apply rules of the Game */
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

    /* Copy the temp world to the main world */
    do row = 1 to rows
        do col = 1 to cols
            world.row.col = temp.row.col
        end
    end
    return

CountNeighbors:
    parse arg row, col
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

