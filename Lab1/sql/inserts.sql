INSERT INTO public."user" (id, name) VALUES (2, 'Bob');
INSERT INTO public."user" (id, name) VALUES (1, 'John Doe');

INSERT INTO public.article (id, name, annotation) VALUES (1, 'name 1', 'ann 1');
INSERT INTO public.article (id, name, annotation) VALUES (2, 'name 2', 'ann 2');

INSERT INTO public.article_feedback (id, article_id, user_id, text) VALUES (2, 2, 2, 'Very bad');
INSERT INTO public.article_feedback (id, article_id, user_id, text) VALUES (1, 1, 1, 'Very nice');

INSERT INTO public.article_view (id, article_id, user_id) VALUES (1, 1, 1);

INSERT INTO public.file (id, name, path, article_id) VALUES (2, 'f2', 'path2', 1);
INSERT INTO public.file (id, name, path, article_id) VALUES (3, 'f3', 'path3', 2);
INSERT INTO public.file (id, name, path, article_id) VALUES (1, 'f1', 'path1', 1);