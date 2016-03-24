module AWSModel where

import qualified SourceModel as S

fw = S.FirewallRule {
    S.fwID = "123"
  , S.outbound = False
  , S.port = "80"
  , S.ip = "0.0.0.0"
  , S.protocol = "tcp"
  }

model = S.Model {
    S.reservations = [res1]
  , S.securityGroups = [sg1, sg2]
}

res1 = S.Reservation {
    S.resID = "res1"
  , S.securityGroupRefs = ["sg1", "sg2"]
  , S.vms = [vm1, vm2, vm3]
  }

vm1 = S.VM {
  S.vmID = "vm1",
  S.vmType = "t2.micro",
  S.load = 1.12,
  S.cost = 0.02,
  S.cpu = 4,
  S.ram = 8,
  S.ami = "abc",
  S.state = 1,
  S.securityGroupRef = "sg-123"
}

vm2 = S.VM {
  S.vmID = "vm2",
  S.vmType = "t2.micro",
  S.load = 0.42,
  S.cost = 0.01,
  S.cpu = 2,
  S.ram = 4,
  S.ami = "abc",
  S.state = 1,
  S.securityGroupRef = "sg-123"
}

vm3 = S.VM {
  S.vmID = "vm3",
  S.vmType = "c2.4xlarge",
  S.load = 10.22,
  S.cost = 0.50,
  S.cpu = 16,
  S.ram = 64,
  S.ami = "abc",
  S.state = 1,
  S.securityGroupRef = "sg-123"
}

sg1 = S.SecurityGroup {
  S.sgID = "sg-1",
  S.vmRefs = ["vm1", "vm2"],
  S.firewallRules = [fw1, fw2]
}

sg2 = S.SecurityGroup {
  S.sgID = "sg-2",
  S.vmRefs = ["vm3"],
  S.firewallRules = [fw1]
}

fw1 = S.FirewallRule {
  S.fwID = "fw-1"
  , S.outbound = False
  , S.port = "80"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  }

fw2 = S.FirewallRule {
  S.fwID = "fw-2"
  , S.outbound = False
  , S.port = "443"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  }

svm2 = [S.VM {S.vmID = "vm1"
             , S.vmType = "t2.micro"
             , S.cost = 2.0e-2
             , S.cpu = 4
             , S.ram = 8
             , S.ami = "abc"
             , S.state = 1
             , S.securityGroupRef = "sg-123"
             , S.load = 1.12}
       ,S.VM {S.vmID = "vm2"
             , S.vmType = "t2.micro"
             , S.cost = 1.0e-2
             , S.cpu = 2
             , S.ram = 4
             , S.ami = "abc"
             , S.state = 1
             , S.securityGroupRef = "sg-123"
             , S.load = 0.42}
       ,S.VM {S.vmID = "vm3"
             , S.vmType = "t4.large"
             , S.cost = 0.0
             , S.cpu = 0
             , S.ram = 0
             , S.ami = "0000"
             , S.state = 0
             , S.securityGroupRef = "sg-123"
             , S.load = 1.32}]
