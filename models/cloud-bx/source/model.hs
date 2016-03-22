data Model = Model [Reservation] [SecurityGroup]
data Reservation = Reservation ID [SecurityGroupRef] [VM]
data SecurityGroupRef = SecurityGroupRef ID Name
data Region = Region Name [SecurityGroup]
data Name = String
data SecurityGroup = SecurityGroup Name [VM][FirewallRule]
data VM = VM ID Type Cost CPU RAM AMI State SecurityGroupRef [FirewallRule]
data ID = String
data Type = String
data Cost = Double
data CPU = Int
data RAM = Int
data AMI = String
data State = Int
data FirewallRule = FirewallRule Outbound Port IP Protocol
data Outbound = Boolean
data Port = String
data IP = String
data Protocol = String