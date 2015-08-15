variable "access_key" {}
variable "secret_key" {}
variable "key_name" {}
variable "key_file" {}

module "a" {
    source = "./tf_replication_node"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    key_name = "${var.key_name}"
    key_file = "${var.key_file}"
    region = "eu-west-1"
}

module "b" {
    source = "./tf_replication_node"
    access_key = "${var.access_key}"
    secret_key = "${var.secret_key}"
    key_name = "${var.key_name}"
    key_file = "${var.key_file}"
    region = "us-east-1"
}

output "node_a" {
    value = "${module.a.ip}"
}

output "node_b" {
    value = "${module.b.ip}"
}
