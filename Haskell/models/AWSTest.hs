module AWSTest where

import qualified AutoScalingModel as AV
import qualified FirewallModel as FV
import qualified SourceModel as S
import qualified RedundancyModel as RV

------------------------------------------------------------------------
-- Source (constructed from `instances.json` and `security-groups.json`)
------------------------------------------------------------------------

source = S.Model {
  S.current = current
  , S.additions = []
  , S.deletions = []
  }

current = S.Current {
  S.reservations = [res1, res2]
  , S.securityGroups = []
  }

res1 = S.Reservation {
  S.resID = "r-e4415446"
  , S.securityGroupRefs = []
  , S.instances = [inst1]
  }

inst1 = S.Instance {
  S.instID = "i-cd33dc69"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.securityGroupRef = "sg-b8d400dc"
  , S.load = 0.00
  }

res2 = S.Reservation {
  S.resID = "r-ff40555d"
  , S.securityGroupRefs = []
  , S.instances = [inst2, inst3, inst4]
  }

inst2 = S.Instance {
  S.instID = "i-6834dbcc"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.securityGroupRef = "sg-77d40013"
  , S.load = 0.00
  }

inst3 = S.Instance {
  S.instID = "i-6a34dbce"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.securityGroupRef = "sg-77d40013"
  , S.load = 0.00
  }

inst4 = S.Instance {
  S.instID = "i-6b34dbcf"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.securityGroupRef = "sg-77d40013"
  , S.load = 0.00
  }

sg1 = S.SecurityGroup {
  S.sgID = "sg-4864f92d"
  , S.instRefs = []
  , S.firewallRules = []
  }

sg2 = S.SecurityGroup {
  S.sgID = "sg-88c92fec"
  , S.instRefs = []
  , S.firewallRules = []
  }

sg3 = S.SecurityGroup {
  S.sgID = "sg-e9195c8c"
  , S.instRefs = []
  , S.firewallRules = []
  }

sg4 = S.SecurityGroup {
  S.sgID = "sg-b8d400dc"
  , S.instRefs = []
  , S.firewallRules = []
  }

sg5 = S.SecurityGroup {
  S.sgID = "sg-77d40013"
  , S.instRefs = []
  , S.firewallRules = []
  }

sg6 = S.SecurityGroup {
  S.sgID = "sg-b9610fdc"
  , S.instRefs = []
  , S.firewallRules = []
  }

sg7 = S.SecurityGroup {
  S.sgID = "sg-b8610fdd"
  , S.instRefs = []
  , S.firewallRules = []
  }
