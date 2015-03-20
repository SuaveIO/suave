(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#load "../../packages/MBrace.Azure.Client/bootstrap.fsx"


open MBrace
open MBrace.Azure
open MBrace.Azure.Client
open MBrace.Azure.Runtime

let config = Unchecked.defaultof<Configuration>

(**

# Using MBrace on Windows Azure 

In this tutorial you will learn how to setup MBrace on Windows Azure.

## Provisioning Using Brisk Engine 

The easiest path to provision an MBrace cluster on Azure using
an Azure Cloud Service is to use [Brisk Engine](http://briskengine.com).
See the [Brisk Engine Tutorial](brisk-tutorial.html).

## Provisioning an Azure Cloud Service Explicitly

In some cases you may decide to provision explicitly, if the options provided
by Brisk Engine do not yet meet your needs.  

If so, see [Creating a Custom Azure Cloud Service which includes MBrace runtime instances](https://github.com/mbraceproject/MBrace.Demos/blob/master/azure/AZURE.md).

For example you may want to:

* adjust the size of VM used for worker instances in your cluster; or

* add endpoints to your cloud service (so your MBrace cluster can publish 
  TCP and HTTP endpoints, either public or to your virtual network, 
  for example, you want your MBrace cluster to publish a web server); or

* enable Remote Access to MBrace cluster worker instances; or

* specify the size of local storage available on MBrace cluster worker instances; or

* upload certificates as part of your provisioning process; or

* specify Azure-specific local caching options; or

* include additional web and worker roles in your cloud service; or

* compile and deploy your own version of the MBrace cluster worker instance software. 


At the time of writing these options were not yet via the Brisk Engine provisioning
service. In this case, you must create and deploy your own Azure cloud service.

In order to provision explicitly, as a prerequisite you need 
to have a Windows Azure account, basic knowledge of the Azure computing and
optionally Microsoft Visual Studio (or any environment supporting F#).


## Deploying as an Azure Web job or explicit VMs

The above deployment techniques use an Azure Cloud Service.
Alternatively you may want to host your MBrace runtime/cluster nodes in one of

* explicit VMs; or

* an Azure web job.

These options are not yet documented but you may be able
to utilize the relevant components used by the
[Custom Azure Cloud Service](https://github.com/mbraceproject/MBrace.Demos/blob/master/azure/AZURE.md).


### Where your MBrace client code runs

Typically the MBrace client will run in:

* an F# interactive instance in your Visual Studio session; or

* as part of another Azure cloud service, website or web job; or

* as a client desktop process.

In all cases, the client will need sufficient network access to be able to write to the
Azure storage and Service Bus accounts used by the MBrace runtime/cluster nodes.

## Using a virtual network 

Any nodes in a MBrace runtime as well as any clients, should be part of the 
same network in order to work properly. 
Because of this restriction in this tutorial you have two choices: you can either 
use one of the virtual machines as a client or you can use
the Windows Azure VPN service to join the virtual network created in Azure 
and access the runtime from a remote client (your on-premises computer).

If you use a virtual network, create and upload certificates and finally configure your VPN client. 
The process is described in [here](http://msdn.microsoft.com/en-us/library/azure/dn133792.aspx).

*)