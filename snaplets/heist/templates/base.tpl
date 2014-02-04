<html>
  <head>
    <title>Analyze by Position Studios</title>
    <link rel="stylesheet" type="text/css" href="/static/screen.css"/>
  </head>
  <body>
    <div id="content">

      <h1>
        Analyze
        <span class="small">
          by
          <a href="http://positionstudios.com">Position Studios</a>
        </span>
        <nonempty couldbe="site-name" tag="${site-name}">
          <span class="small">for</span>
          <site-name/>
        </nonempty>
        <span class="right">
            <ifLoggedIn>
              <a href="/auth/logout">Logout</a>
            </ifLoggedIn>
            <ifLoggedOut>
              <a href="/auth/login">Login</a> |
              <a href="/auth/new_user">Signup</a>
            </ifLoggedOut>
        </span>
      </h1>



      <apply-content/>

    </div>
  </body>
</html>
