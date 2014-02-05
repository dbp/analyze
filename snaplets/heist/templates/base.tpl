<html>
  <head>
    <title>Analyze by Position Studios</title>
    <link rel="stylesheet" type="text/css" href="/static/screen.css"/>
  </head>
  <body>
    <div id="content">

      <h1>
        <div class="title">
          <div class="big">Analyze</div>
          <div class="small">
            by
            <a href="http://positionstudios.com">Position Studios</a>
          </div>
        </div>
        <nonempty couldbe="site-name" tag="${site-name}">
          <div class="site-name">
            <span class="small">for</span>
            <site-name/>
          </div>
        </nonempty>
        <span class="right">
            <ifLoggedIn>
              <a href="/">Home</a> |
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
