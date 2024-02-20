using Microsoft.AspNetCore.HttpOverrides;

var builder = WebApplication.CreateBuilder(args);
builder.Services.AddAuthentication();
var app = builder.Build();
app.UseForwardedHeaders(new ForwardedHeadersOptions
{
    ForwardedHeaders = ForwardedHeaders.XForwardedFor | ForwardedHeaders.XForwardedProto
});
app.UseAuthentication();
app.MapGet("/myapp", () => "Hello World!");

app.Run();

/*
/etc/nginx/sites-available/myapp (need to create symbolic link see blow)
server {
    listen 80;
    server_name 127.0.0.1;

    location /myapp/ {
        proxy_pass http://127.0.0.1:5050/myapp/;  # Replace 5000 with the port your ASP.NET Core app is running on
        proxy_http_version 1.1;
        proxy_set_header   Upgrade $http_upgrade;
        proxy_set_header   Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header   X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header   X-Forwarded-Proto $scheme;
    }
}


    
   35  pwd
   36  ls /opt/tomcat/
   37  ls /opt/tomcat/conf/server.xml
   38  sudo ls /opt/tomcat/conf/server.xml
   39  sudo nano /opt/tomcat/conf/server.xml
   40  sudo apt install nginx
   41  sudo nano /etc/nginx/sites-available/myapp
   42  sudo ln -s /etc/nginx/sites-available/myapp /etc/nginx/sites-enabled/
   43  sudo nginx -t
   44  sudo systemctl restart nginx
   45  sudo ls /etc/ngnix
   46  sudo ls /etc/nginx
   47  cd /etc/ngnix
   48  cd /etc/nginx
   49  ls
   50  sudo nginx
   51  tail -f /var/log/nginx/error.log
   52  sudo ls /etc/nginx/sites-enabled/
   53  sudo nano /etc/nginx/sites-available/myapp
   54  sudo nginx -s reload
   55  sudo nano /etc/nginx/sites-available/myapp
   56  sudo nginx -s reload
   57  sudo service nginx restart
   58  sudo nano /etc/nginx/sites-available/myapp
   59  sudo service nginx restart
   60  ls /var
   61  ls /var/log/nginx
   62  cat  /var/log/nginx/error.log
   63  cat  /var/log/nginx/access.log
   64  ls
   65  cd sites-enabled/
   66  ls
   67  cd myapp
   68  dir
   69  ls
   70  cd myapp
   71  cd ..
   72  cd sites-available/
   73  ls
   74  sudo nano /etc/nginx/sites-available/myapp
   75  sudo service nginx restart
   76  sudo service nginx stop
   77  sudo service nginx start
   78  sudo nano /etc/nginx/sites-available/myapp
   79  sudo service nginx restart
   80  sudo nano /etc/nginx/sites-available/myapp
   81  sudo service nginx start
   82  sudo nginx -t
   83  sudo nano /etc/nginx/sites-available/myapp
   84  sudo nginx -t
   85  sudo nano /etc/nginx/sites-available/myapp
   86* 
   87  sudo service nginx restart
   88  cat  /var/log/nginx/access.log
   89  sudo ls t  /var/log/nginx/
   90  sudo ls t  /var/log/nginx
   91  sudo ls   /var/log/nginx
   92  sudo cat  /var/log/nginx/error.log
   93  sudo service nginx restart
   94  sudo cat  /var/log/nginx/error.log
   95  cat  /var/log/nginx/access.log
   96  head  /var/log/nginx/access.log
   97  sudo nano /etc/nginx/sites-available/myapp
   98  sudo service nginx restart
   99  history
  100  history >c:/mnt/sources/lnet/history.txt
  101  history >/mnt/c/sources/lnet/history.txt
*/