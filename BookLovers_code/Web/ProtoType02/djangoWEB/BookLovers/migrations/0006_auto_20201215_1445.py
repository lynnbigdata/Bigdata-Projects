# Generated by Django 3.0.3 on 2020-12-15 05:45

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('BookLovers', '0005_librarybook'),
    ]

    operations = [
        migrations.AddField(
            model_name='librarybook',
            name='Ratings_Dist',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='author',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='average_rating',
            field=models.FloatField(default=0),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='bookImageUrl',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='bookname',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='loan_count',
            field=models.IntegerField(default=0),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='num_pages',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='publication_year',
            field=models.IntegerField(default=0),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='publisher',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='text_reviews_count',
            field=models.IntegerField(default=0),
        ),
        migrations.AddField(
            model_name='librarybook',
            name='vol',
            field=models.IntegerField(default=0),
        ),
    ]
