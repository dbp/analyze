<apply template="base">

  <ifLoggedIn>
    <p>
      This is a simple demo page served using
      <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
      and the <a href="http://snapframework.com/">Snap</a> web framework.
    </p>

    <p>Congrats!  You're logged in as '<loggedInUser/>'</p>

    <p><a href="/auth/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <a href="/auth/login">Login</a>
  </ifLoggedOut>

</apply>
