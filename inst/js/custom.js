

$(document).on("shiny:sessioninitialized", function(event) {

    $(".custom-nav .nav-tabs").addClass("nav-justified");


    $(".sidebar-menu li").on("click", function () {
      /*          setTimeout(function () {
    $(".nav-tabs").addClass("nav-justified");
          },300);*/
      var intx = 0;
      var interval = setInterval(function () {
        $(".nav-tabs").addClass("nav-justified");
        if (++intx == 3) {
          window.clearInterval(interval);
        }
      }, 1000);
    });

/*
    $(document).on("click",'[id*=txtUsername_search]',function (e) {
     $('[id*=-userList]').show(700);
    });

    $(document).on("keyup",'[id*=txtUsername_text]',function (e) {
      var lsval = e.key;
      if(lsval === "Enter"){
         $('[id*=-userList]').show(700);
      }
    });*/

  });
