import main from "./tojs_example_test.mjs";

let result = await main({
  siteTitle: "My Blog",
  blogPosts: [
    {
      title: "My second post",
      author: { name: "John" },
      image: { src: "./example.jpg", alt: "An example image" },
      content:
        "<p>Vel esse sequi consectetur qui debitis doloribus facilis molestiae. Et sunt dolores dolorum possimus molestiae et mollitia. Illum doloremque laborum tempore. Officiis consequuntur architecto ut deserunt nihil fuga et. Et dicta dolor sed sunt. Dolorem sequi dolorem veritatis.</p>\n\n<p>Laborum aut aliquam non facilis nesciunt et commodi. Voluptatem quis animi voluptate. Expedita quia dolores culpa molestiae quis. Fugit et quia distinctio est repellendus pariatur. Aut ducimus quae repellat sint praesentium odio et.</p>\n\n<p>Vero in dolor omnis unde. Impedit placeat laborum architecto consectetur inventore doloribus in. Sunt veritatis quam sequi ut. Et sapiente earum necessitatibus libero dolorem totam et consequuntur. Dolor voluptatibus exercitationem dolorum voluptatum voluptatibus. Nostrum nostrum consequatur dolores et voluptatem voluptas.</p>\n\n<p>Dolores labore qui et autem reprehenderit unde. Blanditiis quia aut explicabo. Quae sequi nostrum quasi dolore. Quis quis commodi enim fugit veniam. Id veniam est aspernatur.</p>\n\n<p>Voluptate qui vero cupiditate earum enim et fugiat odio. Voluptas tenetur qui nihil voluptatem animi architecto facilis. Occaecati atque molestiae praesentium in ut. Earum minima et minima autem quod saepe aut.</p>",
      date: "2023-04-16T19:10:31.456Z",
    },
    {
      title: "My first post",
      author: { name: "John" },
      image: null,
      content:
        "<p>Enim qui repellendus dolore dolor autem. Quaerat voluptatum maiores repellendus. Omnis sit in excepturi. Qui molestias ducimus maiores illo quos corrupti ad. Autem ratione accusamus quia. Ad incidunt quia rerum fuga quisquam eligendi dolor.</p>\n\n<p>Vel deleniti alias cum vitae. Nihil a in minima provident. Occaecati magnam quia vitae itaque explicabo reiciendis quidem. Deleniti alias ut dolorum cum reprehenderit sequi et mollitia. Ut ullam eum enim.</p>\n\n<p>Quisquam dicta et quidem et voluptates odit magnam est. Maxime distinctio dolor delectus. Facere itaque labore voluptates eum est. Eos pariatur omnis rerum earum molestias quia et. Ut non est laudantium et. Assumenda fugiat perspiciatis minus.</p>\n\n<p>Excepturi dolorum praesentium quo. Incidunt id similique odit fugit eum adipisci delectus. Velit ipsum non tenetur impedit et harum quia consequatur.</p>\n\n<p>Eum nam velit consectetur iste. Odit rerum cupiditate dignissimos. Expedita nobis veniam atque dolor qui. Ut eius laudantium ut expedita nobis repellat optio autem.</p>",
      date: "2023-04-16T19:10:10.487Z",
    },
  ],
});

console.log(result);