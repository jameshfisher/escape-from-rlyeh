---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                         --
--  ▓█████   ██████  ▄████▄   ▄▄▄       ██▓███  ▓█████      █████▒██▀███   ▒█████   ███▄ ▄███▓    ██▀███ █▒ ██▓   ▓██   ██▓▓█████  ██░ ██  --
--  ▓█   ▀ ▒██    ▒ ▒██▀ ▀█  ▒████▄    ▓██░  ██▒▓█   ▀    ▓██   ▒▓██ ▒ ██▒▒██▒  ██▒▓██▒▀█▀ ██▒   ▓██ ▒ ██▒ ██▒    ▒██  ██▒▓█   ▀ ▓██░ ██▒  --
--  ▒███   ░ ▓██▄   ▒▓█    ▄ ▒██  ▀█▄  ▓██░ ██▓▒▒███      ▒████ ░▓██ ░▄█ ▒▒██░  ██▒▓██    ▓██░   ▓██ ░▄█ ▒▒██░     ▒██ ██░▒███   ▒██▀▀██░  --
--  ▒▓█  ▄   ▒   ██▒▒▓▓▄ ▄██▒░██▄▄▄▄██ ▒██▄█▓▒ ▒▒▓█  ▄    ░▓█▒  ░▒██▀▀█▄  ▒██   ██░▒██    ▒██    ▒██▀▀█▄  ▒██░     ░ ▐██▓░▒▓█  ▄ ░▓█ ░██   --
--  ░▒████▒▒██████▒▒▒ ▓███▀ ░ ▓█   ▓██▒▒██▒ ░  ░░▒████▒   ░▒█░   ░██▓ ▒██▒░ ████▓▒░▒██▒   ░██▒   ░██▓ ▒██▒░██████▒ ░ ██▒▓░░▒████▒░▓█▒░██▓  --
--  ░░ ▒░ ░▒ ▒▓▒ ▒ ░░ ░▒ ▒  ░ ▒▒   ▓▒█░▒▓▒░ ░  ░░░ ▒░ ░    ▒ ░   ░ ▒▓ ░▒▓░░ ▒░▒░▒░ ░ ▒░   ░  ░   ░ ▒▓ ░▒▓░░ ▒░▓  ░  ██▒▒▒ ░░ ▒░ ░ ▒ ░░▒░▒  --
--   ░ ░  ░░ ░▒  ░ ░  ░  ▒     ▒   ▒▒ ░░▒ ░      ░ ░  ░    ░       ░▒ ░ ▒░  ░ ▒ ▒░ ░  ░      ░     ░▒ ░ ▒░░ ░ ▒  ░▓██ ░▒░  ░ ░  ░ ▒ ░▒░ ░  --
--     ░   ░  ░  ░  ░          ░   ▒   ░░          ░       ░ ░     ░░   ░ ░ ░ ░ ▒  ░      ░        ░░   ░   ░ ░   ▒ ▒ ░░     ░    ░  ░░ ░  --
--     ░  ░      ░  ░ ░            ░  ░            ░  ░             ░         ░ ░         ░         ░         ░  ░░ ░        ░  ░ ░  ░  ░  --
--                  ░                                                                                             ░ ░                      --
--                                                                                                                                         --
--   ╔═╗  ╦  ╔═╗╦  ╦╔═╗╔═╗╦═╗╔═╗╔═╗╔╦╗╦╔═╗╔╗╔  ╔═╗╔═╗╔╦╗╔═╗  ╔═╗╔═╗  ╔╗╔╔═╗╔╗╔   ╔═╗╦ ╦╔═╗╦  ╦╔╦╗╔═╗╔═╗╔╗╔  ╔═╗╔═╗╔═╗╔╦╗╔═╗╔╦╗╦═╗╦╔═╗╔═╗   --
--   ╠═╣  ║  ║ ║╚╗╔╝║╣ ║  ╠╦╝╠═╣╠╣  ║ ║╠═╣║║║  ║ ╦╠═╣║║║║╣   ║ ║╠╣   ║║║║ ║║║║───║╣ ║ ║║  ║  ║ ║║║╣ ╠═╣║║║  ║ ╦║╣ ║ ║║║║║╣  ║ ╠╦╝║║╣ ╚═╗   --
--   ╩ ╩  ╩═╝╚═╝ ╚╝ ╚═╝╚═╝╩╚═╩ ╩╚   ╩ ╩╩ ╩╝╚╝  ╚═╝╩ ╩╩ ╩╚═╝  ╚═╝╚    ╝╚╝╚═╝╝╚╝   ╚═╝╚═╝╚═╝╩═╝╩═╩╝╚═╝╩ ╩╝╚╝  ╚═╝╚═╝╚═╝╩ ╩╚═╝ ╩ ╩╚═╩╚═╝╚═╝   --
--                                                                                                                                         --
---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

import Color (black)
import Graphics.Element (Element, color, spacer, height, width, flow, right, down)
import Text (centered, fromString)
import List
import Keyboard (KeyCode, isDown)
import Signal (Signal, dropRepeats, foldp, mergeMany, keepIf, map, map2)

lookup : eq -> List (eq,v) -> Maybe v
lookup needle l = case l of
  [] -> Nothing
  (k,v)::l' -> if needle == k
               then Just v
               else lookup needle l'

lookupBi : eq -> List (eq,eq) -> Maybe eq
lookupBi needle l = case lookup needle l of
  Just v -> Just v
  Nothing -> lookup needle (List.map (\(a,b) -> (b,a)) l)

-- Number of 90-degree turns clockwise from north/front.
-- These are equal modulo 4.
-- Think of as north/east/south/west:
--   north/top/front   = 0 = 4 = ...
--   east/right        = 1 = 5 = ...
--   south/down/bottom = 2 = 6 = ...
--   west/left         = 3 = 7 = ...
type alias Direction = Int

-- "normalize direction"
nd : Direction -> Direction
nd o = o % 4

rotateLeft : Direction -> Direction
rotateLeft d = nd (d - 1)

reverse : Direction -> Direction
reverse d = nd (d - 2)

localDirectionToGlobal : Direction -> Direction -> Direction
localDirectionToGlobal global local = nd (global + local)

-- We model the world as a graph. This allows for complex, confusing
-- topologies. A vertex in this graph is a medium-size square-shaped
-- area of the world. We identify them by integers.
type alias Vertex = Int

-- Not your grand-daddy's graph. Vertices can be seen as squares, and
-- edges as lines between the sides of the squares; not between the
-- squares themselves. Each square is part of the area of the
-- world. Edges define how those areas link up.
--
-- On orientation: imagine the squares as embedded in the plane. Then
-- a square's north side is on the top, its east side is to the right,
-- etc.
type alias Graph = {
  vertices: List Vertex,
  edges:    List ((Vertex,Direction), (Vertex,Direction))
}

-- if I am on vertex v, facing direction d, what is in local direction
-- d', and which global direction would I be facing if I were to walk
-- to it?
neighbor : Graph -> Vertex -> Direction -> Direction -> Maybe (Vertex,Direction)
neighbor {edges} v d d' = lookupBi (v, localDirectionToGlobal d d') edges

-- as in "forward, left, forward ..."
type alias Path = List Direction

followPath : Graph -> Vertex -> Direction -> Path -> Maybe (Vertex,Direction)
followPath g v d p = case p of
  [] -> Just (v,d)
  m::p' -> case neighbor g v d m of
    Nothing -> Nothing
    Just (v',d') -> followPath g v' d' p'


-- Graphs are "locally planar". The topology is locally Euclidean and
-- consistent. "Local" means the neighborhood of the current
-- vertex. The neighborhood is the current vertex and its eight
-- "surrounding" vertices. Direction is given in the vertex's
-- "local" orientation: seen from the perspective of the current
-- vertex which is facing north, which direction is the neighbor
-- facing?
type alias Neighborhood = List (List (Maybe (Vertex,Direction)))

neighborhoodOf : Graph -> Vertex -> Direction -> Neighborhood
neighborhoodOf g v d = let f = followPath g v d in
 [ [ f [0,3] , f [0] , f [0,1]  ]
 , [ f [3  ] , f [ ] , f [1  ]  ]
 , [ f [2,1] , f [2] , f [2,3]  ]
 ]


-- A graph is consistent iff for every vertex x, going north then east
-- yields the same as going east then north (and analogously for
-- north/west, south/east, etc).
--
-- This ensures that drawing the local part of the graph is
-- unambiguous. It rejects some crazy graphs as invalid. It is a lot
-- less strict than Euclidean/planar though!
consistent : Graph -> Bool
consistent g = True  -- TODO



type alias State = {
  graph:Graph,  -- the world at the moment
  vertex:Int,   -- player position in the graph
  dir:Direction -- which way is the player facing on the vertex
}


showNeighborhood : Neighborhood -> Element
showNeighborhood =
  let f c = case c of
    Nothing -> color black (spacer 30 30)
    Just (v,d) -> height 30 (width 30 (centered (fromString (toString v))))  -- TODO rotate it
  in List.map (List.map f >> flow right) >> flow down

showState : State -> Element
showState {graph,vertex,dir} = showNeighborhood (neighborhoodOf graph vertex dir)

-- a simple planar graph.
--  +-+
--  |1|
--  +-+
--  |2|
--  +-+
l1graph : Graph
l1graph = { vertices = [ 1, 2 ]
          , edges =    [ ((1,2),(2,0))
                       ]
          }

l1 : State
l1 =
 { graph = l1graph
 , vertex = 1
 , dir = 0
 }

--    .....
--    :   :
--   +-+  :
--   |1|  :
--   +-+  :
--    :   :
--    :...:
l2 : State
l2 =
 { graph = { vertices = [1]
           , edges = [ ((1,0), (1,2)) ]
           }
 , vertex = 1
 , dir = 0
 }

-- ╔════════╗
-- ║        ║
-- ║ +-+-+ +-+
-- ╚═|1|2| |6|
--   +-+-+-+-+
--     |3|4|5|
--     +-+-+-+
l3 : State
l3 =
 { graph = { vertices = [1,2,3,4,5,6]
           , edges = [ ((1,1),(2,3))
                     , ((2,2),(3,0))
                     , ((3,1),(4,3))
                     , ((4,1),(5,3))
                     , ((5,0),(6,2))
                     , ((6,0),(1,3))
                     ]
           }
 , vertex = 1
 , dir = 0
 }

type alias Action = Direction

keypresses : KeyCode -> Signal ()
keypresses c = isDown c |> keepIf identity False |> map (always ())

actions : Signal Action
actions =
  let
    f : KeyCode -> Direction -> Signal Action
    f kc p = keypresses kc |> map (always p)
  in mergeMany
     [ f 38 0
     , f 40 2
     , f 37 3
     , f 39 1
     ]

step : Action -> State -> State
step a {graph,vertex,dir} = case neighbor graph vertex dir a of
  Nothing -> {graph=graph, vertex=vertex, dir=dir}
  Just (v',d') -> {graph=graph, vertex=v', dir=dir} -- dir or d' ? I think we want to maintain global orientation

state : Signal State
state = foldp step l3 actions

main : Signal Element
main = map showState state
