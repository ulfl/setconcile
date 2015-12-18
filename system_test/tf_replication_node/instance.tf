variable "access_key" {}
variable "secret_key" {}
variable "key_name" {}
variable "key_file" {}
variable "region" { }
variable "amis" {
    default = {
#        eu-west-1 = "ami-8d7241fa" # ddc4415-1M-0.01-1024-nodea
#        us-east-1 = "ami-b1bcf6d4" # ddc4415-1M-0.01-1024-nodeb.
#        us-east-1 = "ami-c980c8ac" # 16bf07f-1M-none-1024-nodeb.
#        eu-west-1 = "ami-9382b3e4" # 16bf07f-1M-all-1024-nodea.
#        us-east-1 = "ami-6957190c" # b8cdaac-1M-none-1024-nodeb.
#        eu-west-1 = "ami-7b75440c" # b8cdaac-1M-all-1024-nodea.
#        eu-west-1 = "ami-13665164" # b7bfacc-1M-0.01-1024-nodea.
#        us-east-1 = "ami-7d1f5d18" # b7bfacc-1M-0.01-1024-nodeb.
#        eu-west-1 = "ami-7be2d60c" # f5cd04b-1M-0.1-1024-nodea.
#        us-east-1 = "ami-eb22618e" # f5cd04b-1M-0.1-1024-nodeb.
#        eu-west-1 = "ami-07596d70" # 0679086-1M-0.001-1024-nodea.
#        us-east-1 = "ami-f5357590" # 0679086-1M-0.001-1024-nodeb.
#        eu-west-1 = "ami-fda2888a" # eec22ce-1M-0.1-4096-nodea.
#        us-east-1 = "ami-332d6a56" # eec22ce-1M-0.1-4096-nodeb.
#        eu-west-1 = "ami-9bc8e3ec" # f862c1e-1M-0.1-4096-nodea.
#        us-east-1 = "ami-51ef9534" # f862c1e-1M-0.1-4096-nodeb.
#        eu-west-1 = "ami-c796beb0" # 12d3e70-1M-0.001-4096-nodea.
#        us-east-1 = "ami-4998e32c" # 12d3e70-1M-0.001-4096-nodeb.
        eu-west-1 = "ami-92401ce5" # ubuntu 14.04.3.
        us-east-1 = "ami-2dcf7b46" # ubuntu 14.04.3.
    }
}

provider "aws" {
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    region = "${var.region}"
}

resource "aws_instance" "replication_node" {
    ami = "${lookup(var.amis, var.region)}"
    instance_type = "m4.large"
    key_name = "${var.key_name}"
    security_groups = ["${aws_security_group.my_sec_group.id}"]
    subnet_id = "${aws_subnet.public_subnet.id}"
    associate_public_ip_address = true
    tags { Name = "replication-test" }

    root_block_device {
        volume_type = "gp2"
        volume_size = 500
    }
    
    provisioner "file" {
        connection {user = "ubuntu" key_file = "${var.key_file}"}
        source = "${path.module}/riak.patch"
        destination = "riak.patch"
    }

    provisioner "remote-exec" {
        connection {user = "ubuntu" key_file = "${var.key_file}"}
        script = "${path.module}/setup.sh"
    }

    # Run separately so that the limits.conf changes are in affect.
    provisioner "remote-exec" {
        connection {user = "ubuntu" key_file = "${var.key_file}"}
        script = "${path.module}/start_riak.sh"
    }
}

output "ip" {
    value = "${aws_instance.replication_node.public_ip}"
}
