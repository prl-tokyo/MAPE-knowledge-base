module AWSModel(Model)
where

model = Model {
mVMS = [vm1, vm2, vm3, vm4],
mSecurityGroup = [sg1, sg2]
}
vm1 = VirtualMachine {
svmID = "vm1",
svmType = "t2.micro",
svmLoad = 1.12,
svmCost = 0.02,
svmCPU = 4,
svmRAM = 8,
svmAMI = "abc",
svmState = 1,
svmSecurityGroupRef = "sg-123",
svmFirewallRules = []
}

vm2 = VirtualMachine {
svmID = "vm2",
svmType = "t2.micro",
svmLoad = 0.42,
svmCost = 0.01,
svmCPU = 2,
svmRAM = 4,
svmAMI = "abc",
svmState = 1,
svmSecurityGroupRef = "sg-123",
svmFirewallRules = []
}

vm3 = VirtualMachine {
svmID = "vm3",
svmType = "c2.4xlarge",
svmLoad = 10.22,
svmCost = 0.50,
svmCPU = 16,
svmRAM = 64,
svmAMI = "abc",
svmState = 1,
svmSecurityGroupRef = "sg-123",
svmFirewallRules = []
}

sg1 = SecurityGroup {
}

sg2 = SecurityGroup {
}
