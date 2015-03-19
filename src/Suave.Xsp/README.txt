
System.UnauthorizedAccessException
Access to the path "/etc/mono/registry" is denied.

http://stackoverflow.com/questions/24872394/access-to-the-path-etc-mono-registry-is-denied


simply creating the folder (/etc/mono/registry) and setting the right permissions (chmod uog+rw /etc/mono/registry) did the trick. 

