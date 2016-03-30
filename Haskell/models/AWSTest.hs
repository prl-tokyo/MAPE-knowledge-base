module AWSTest where

import qualified AutoScalingModel as AV
import qualified FirewallModel as FV
import qualified SourceModel as S
import qualified RedundancyModel as RV

------------------------------------------------------------------------
-- Source (constructed from `instances.json` and `security-groups.json`)
------------------------------------------------------------------------

source = S.Model {
  S.instances = [inst1, inst2, inst3, inst4]
  , S.securityGroups = [sg1, sg2, sg3, sg4, sg5]
  , S.instanceTypes = [t1, t2, t3, t4, t5, t6, t7
                    , t8, t9, t10, t11, t12, t13, t14]
  }

inst1 = S.Instance {
  S.instID = "i-cd33dc69"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.instStatus = 0
  , S.securityGroupRef = "sg-b8d400dc"
  , S.load = 0.00
  }

inst2 = S.Instance {
  S.instID = "i-6834dbcc"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.instStatus = 0
  , S.securityGroupRef = "sg-77d40013"
  , S.load = 0.00
  }

inst3 = S.Instance {
  S.instID = "i-6a34dbce"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.instStatus = 0
  , S.securityGroupRef = "sg-77d40013"
  , S.load = 0.00
  }

inst4 = S.Instance {
  S.instID = "i-6b34dbcf"
  , S.instType = "t2.micro"
  , S.ami = "ami-59bdb937"
  , S.state = 16
  , S.instStatus = 0
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
  , S.firewallRules = [fw1, fw2, fw3]
  }

fw1 = S.FirewallRule {
  S.fwRuleID = "fw1"
  , S.outbound = True
  , S.port = "80"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

fw2 = S.FirewallRule {
  S.fwRuleID = "fw2"
  , S.outbound = True
  , S.port = "22"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

fw3 = S.FirewallRule {
  S.fwRuleID = "fw3"
  , S.outbound = True
  , S.port = "443"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

sg3 = S.SecurityGroup {
  S.sgID = "sg-e9195c8c"
  , S.instRefs = []
  , S.firewallRules = [fw4]
  }

fw4 = S.FirewallRule {
  S.fwRuleID = "fw4"
  , S.outbound = True
  , S.port = "22"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

sg4 = S.SecurityGroup {
  S.sgID = "sg-b8d400dc"
  , S.instRefs = []
  , S.firewallRules = [fw5, fw6]
  }

fw5 = S.FirewallRule {
  S.fwRuleID = "fw5"
  , S.outbound = True
  , S.port = "22"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

fw6 = S.FirewallRule {
  S.fwRuleID = "fw6"
  , S.outbound = True
  , S.port = "3306"
  , S.ip = "sg-77d40013"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

sg5 = S.SecurityGroup {
  S.sgID = "sg-77d40013"
  , S.instRefs = []
  , S.firewallRules = [fw7, fw8, fw9]
  }

fw7 = S.FirewallRule {
  S.fwRuleID = "fw7"
  , S.outbound = True
  , S.port = "80"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

fw8 = S.FirewallRule {
  S.fwRuleID = "fw8"
  , S.outbound = True
  , S.port = "22"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

fw9 = S.FirewallRule {
  S.fwRuleID = "fw9"
  , S.outbound = True
  , S.port = "443"
  , S.ip = "0.0.0.0/0"
  , S.protocol = "tcp"
  , S.fwStatus = 0
  }

t1 = S.InstanceType {
  S.typeID = "t2.nano"
  , S.typeCPUs = 1
  , S.typeRAM = 0.5
  , S.typeCost = 0.01
  }

t2 = S.InstanceType {
  S.typeID = "t2.micro"
  , S.typeCPUs = 1
  , S.typeRAM = 1.00
  , S.typeCost = 0.02
  }

t3 = S.InstanceType {
  S.typeID = "t2.small"
  , S.typeCPUs = 1
  , S.typeRAM = 2.00
  , S.typeCost = 0.04
  }

t4 = S.InstanceType {
  S.typeID = "t2.medium"
  , S.typeCPUs = 2
  , S.typeRAM = 4.00
  , S.typeCost = 0.08
  }

t5 = S.InstanceType {
  S.typeID = "t2.large"
  , S.typeCPUs = 2
  , S.typeRAM = 8.00
  , S.typeCost = 0.16
  }

t6 = S.InstanceType {
  S.typeID = "m4.large"
  , S.typeCPUs = 2
  , S.typeRAM = 8.00
  , S.typeCost = 0.174
  }

t7 = S.InstanceType {
  S.typeID = "m4.xlarge"
  , S.typeCPUs = 4
  , S.typeRAM = 16.00
  , S.typeCost = 0.348
  }

t8 = S.InstanceType {
  S.typeID = "m4.2xlarge"
  , S.typeCPUs = 8
  , S.typeRAM = 32.00
  , S.typeCost = 0.695
  }

t9 = S.InstanceType {
  S.typeID = "m4.4xlarge"
  , S.typeCPUs = 16
  , S.typeRAM = 64.00
  , S.typeCost = 1.391
  }

t10 = S.InstanceType {
  S.typeID = "m4.10xlarge"
  , S.typeCPUs = 40
  , S.typeRAM = 160.00
  , S.typeCost = 3.477
  }

t11 = S.InstanceType {
  S.typeID = "m3.medium"
  , S.typeCPUs = 1
  , S.typeRAM = 3.75
  , S.typeCost = 0.096
  }

t12 = S.InstanceType {
  S.typeID = "m3.large"
  , S.typeCPUs = 2
  , S.typeRAM = 7.5
  , S.typeCost = 0.193
  }

t13 = S.InstanceType {
  S.typeID = "m3.xlarge"
  , S.typeCPUs = 4
  , S.typeRAM = 15.00
  , S.typeCost = 0.385
  }

t14 = S.InstanceType {
  S.typeID = "m3.2xlarge"
  , S.typeCPUs = 8
  , S.typeRAM = 30.00
  , S.typeCost = 0.77
  }
