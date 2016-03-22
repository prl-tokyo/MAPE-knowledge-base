data FirewallModel = FirewallModel [Node] [(Node, Node, [(Port, Protocol)])]
data Node = Node Name
data Name = String
data Port = Int
data Protocol = String
