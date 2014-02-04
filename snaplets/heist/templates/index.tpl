<apply template="base">
  <ifLoggedIn>
    <h4>Sites <span class="right"><a href="/site/new">New Site</a></span></h4>

    <sites>
      <p>
        <a href="/site/${id}"><name/> (<url/>)</a>
      </p>
    </sites>

  </ifLoggedIn>

  <ifLoggedOut>
    <p>Curious what this is? Dig in to the source at: <a href="https://github.com/dbp/analyze">https://github.com/dbp/analyze</a> and a client for Haskell/Snap: <a href="https://github.com/dbp/analyze-client">https://github.com/dbp/analyze-client</a>.
    </p>

    <p>No idea what any of that means? You probably want to visit <a href="http://positionstudios.com">http://positionstudios.com</a> instead.</p>
  </ifLoggedOut>

</apply>
