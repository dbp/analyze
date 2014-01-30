<apply template="base">

  <ifLoggedIn>
    <a href="/site/new">New Site</a><br/>
    <sites>
      <p>
        <a href="/site/${id}"><name/> (<url/>)</a>
      </p>
    </sites>

    <p><a href="/auth/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <a href="/auth/login">Login</a><br/>
    <a href="/auth/new_user">Signup</a>
  </ifLoggedOut>

</apply>
